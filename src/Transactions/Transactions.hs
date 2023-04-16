{-# LANGUAGE OverloadedStrings #-}

module Transactions.Transactions where

import Control.Concurrent.STM
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Validation
import Network.Run.TCP
import Network.Socket
import System.IO
import Text.Read

transactionsMain :: IO ()
transactionsMain = do
  server <- newServer
  runTCPServer (Just "localhost") (show port) $ \sock -> do
    handle <- socketToHandle sock ReadWriteMode
    talk handle server

port :: PortNumber
port = 44444

type ClientName = String

data Client =
  Client
    { name :: String
    , clientHandle :: Handle
    , balance :: TVar Int
    , password :: String
    , loggedIn :: TVar Bool
    , messages :: TQueue String
    }

newtype Server =
  Server
    { clients :: TVar (Map.Map ClientName Client)
    }

data ClientAction
  = Transfer
  | ShowBalance
  | Deposit
  | Withdraw
  | LogOut
  deriving (Show, Read, Enum, Bounded)

talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  client <- start
  clientAction server client
  where
    start = do
      hPutStrLn handle "Select 'signUp' or 'logIn'"
      startCommand <- hGetLine handle
      if startCommand == "signUp"
        then addClient handle server
        else if startCommand == "logIn"
               then logIn handle server
               else do
                 hPutStrLn handle "Invalid command"
                 start

clientAction :: Server -> Client -> IO ()
clientAction server client = do
  let options :: [ClientAction]
      options = [minBound .. maxBound]
  printClientMessage client $
    "Select one of the following: " ++ intercalate ", " (map show options)
  response <- hGetLine (clientHandle client)
  let maybeAction = readMaybe response
  case maybeAction of
    Nothing -> do
      printClientMessage client "Invalid response. Try again"
      clientAction server client
    Just action -> do
      runClientAction server client action
      clientAction server client

runClientAction :: Server -> Client -> ClientAction -> IO ()
runClientAction server client action =
  case action of
    Transfer -> transferWithError server client
    ShowBalance -> showBalance client
    Deposit -> deposit client
    Withdraw -> withdraw client
    LogOut -> logOut server client

newServer :: IO Server
newServer =
  atomically $ do
    emptyClients <- newTVar Map.empty
    return $ Server emptyClients

addClient :: Handle -> Server -> IO Client
addClient handle server = do
  hPutStrLn handle "What is your name?"
  clientName <- enterName
  hPutStrLn handle "Choose your password"
  psw <- enterPassword
  hPutStrLn handle "How much would you like to deposit?"
  initialBalance <- getAmount handle
  client <-
    atomically (addClientSTM handle server clientName psw initialBalance)
  printClientMessage client $ clientName ++ " successfully added"
  return client
  where
    enterName = do
      clientName <- hGetLine handle
      if null clientName
        then do
          hPutStrLn handle "Name must be non null. Try again"
          enterName
        else do
          maybeClient <- atomically $ getClientSTM server clientName
          case maybeClient of
            Success _ -> do
              hPrint handle (ClientAlreadyExists clientName)
              hPutStrLn handle "Try again"
              enterName
            Failure _ -> return clientName
    enterPassword = do
      psw <- hGetLine handle
      if length psw < 5
        then do
          hPutStrLn handle "Password must be 5 characters or more. Try again"
          enterPassword
        else return psw

-- requires that the client does not already exist
addClientSTM :: Handle -> Server -> ClientName -> String -> Int -> STM Client
addClientSTM handle server clientName psw initialBalance = do
  newBalance <- newTVar initialBalance
  loggedInBool <- newTVar True
  emptyMessages <- newTQueue
  let newClient =
        Client clientName handle newBalance psw loggedInBool emptyMessages
  modifyTVar (clients server) (Map.insert clientName newClient)
  return newClient

logIn :: Handle -> Server -> IO Client
logIn handle server = do
  client <- getClient handle server
  printClientMessage client "Enter password:"
  enterPassword client
  where
    enterPassword client = do
      psw <- hGetLine handle
      if psw == password client
        then do
          printClientMessage client "Successful login"
          _ <- atomically $ writeTVar (loggedIn client) True
          printAllMessages client
          return client
        else do
          printClientMessage client "Incorrect passord. Try again"
          enterPassword client

logOut :: Server -> Client -> IO ()
logOut server client = do
  _ <- atomically $ writeTVar (loggedIn client) False
  talk (clientHandle client) server

getAmount :: Handle -> IO Int
getAmount handle = enterAmount
  where
    enterAmount = do
      amountStr <- hGetLine handle
      let maybeAmount = readMaybe amountStr :: Maybe Int
      case maybeAmount of
        Just amount ->
          if amount > 0
            then return amount
            else do
              hPutStrLn handle "Balance must be greater than zero. Try again"
              enterAmount
        Nothing -> do
          hPutStrLn handle "Invalid number. Try again"
          enterAmount

getClient :: Handle -> Server -> IO Client
getClient handle server = do
  hPutStrLn handle "Enter username:"
  clientName <- hGetLine handle
  maybeClient <- atomically $ getClientSTM server clientName
  case maybeClient of
    Success client -> return client
    Failure txnError -> do
      hPrint handle txnError
      getClient handle server

getClientSTM :: Server -> ClientName -> STMReturnType Client
getClientSTM server clientName = do
  currentClients <- readTVar (clients server)
  case Map.lookup clientName currentClients of
    Just client -> return $ Success client
    Nothing -> return $ Failure (ClientDoesNotExist clientName)

removeClient :: Handle -> Server -> ClientName -> IO ()
removeClient handle server newClient =
  runSTM
    handle
    (removeClientSTM server newClient)
    (\_ -> newClient ++ " successfully deleted")

removeClientSTM :: Server -> ClientName -> STMReturnType ()
removeClientSTM server clientToRemove = do
  currentClients <- readTVar (clients server)
  if Map.notMember clientToRemove currentClients
    then return $ Failure (ClientDoesNotExist clientToRemove)
    else do
      modifyTVar (clients server) (Map.delete clientToRemove)
      return $ Success ()

-- payDebt :: Handle -> Server -> ClientName -> ClientName -> Int -> IO ()
-- payDebt handle server startClient endClient amount = do
--   maybeRemainder <-
--     atomically $ transferReturningRemainder server startClient endClient amount
--   case maybeRemainder of
--     Failure errorMessage -> hPrint handle errorMessage
--     Success Nothing -> hPutStrLn handle "Successful transaction"
--     Success (Just remainder) -> do
--       hPutStrLn
--         handle
--         ("Insufficient funds to make complete transfer. The account has been emptied and the remaining balance " ++
--          show remainder ++ " will be taken where there are sufficient funds")
--       transferWithRetry handle server startClient endClient remainder
transferWithError :: Server -> Client -> IO ()
transferWithError server startClient = do
  printClientMessage startClient "Who would you like to transfer with?"
  endClient <- getClient (clientHandle startClient) server
  printClientMessage startClient "How much would you like to send?"
  amount <- getAmount (clientHandle startClient)
  maybeTransfer <- atomically (transferSTM False startClient endClient amount)
  case maybeTransfer of
    Success _ -> do
      printClientMessage startClient $
        "Successfully sent money to " ++ name endClient
      sendMessage endClient $
        "Received " ++ show amount ++ " from " ++ name startClient
    Failure txnError -> printClientMessage startClient (show txnError)

-- transferWithRetry ::
--      Handle -> ClientName -> ClientName -> Int -> IO ()
-- transferWithRetry handle startClient endClient amount =
--   runSTM
--     handle
--     (transferSTM True startClient endClient amount)
--     (\_ -> "Successful transaction from " ++ startClient ++ " to " ++ endClient)
-- transferReturningRemainder ::
--      Server -> ClientName -> ClientName -> Int -> STMReturnType (Maybe Int)
-- transferReturningRemainder server startClient endClient amount = do
--   _ <- depositSTM server endClient amount
--   validatedBalance <- showBalanceSTM server startClient
--   case validatedBalance of
--     Failure errorMessage -> return $ Failure errorMessage
--     Success currentBalance ->
--       if currentBalance >= amount
--         then do
--           _ <- transferSTM False server startClient endClient amount
--           return $ Success Nothing
--         else do
--           _ <- transferSTM False server startClient endClient currentBalance
--           return $ Success (Just (amount - currentBalance))
transferSTM ::
     Bool -- retry transaction
  -> Client
  -> Client
  -> Int
  -> STMReturnType ()
transferSTM retryIfInsufficientFunds startClient endClient amount = do
  _ <- depositSTM endClient amount
  withdrawSTM retryIfInsufficientFunds startClient amount

deposit :: Client -> IO ()
deposit client = do
  printClientMessage client "How much would you like to deposit?"
  amount <- getAmount (clientHandle client)
  _ <- atomically $ depositSTM client amount
  printClientMessage client $ "Successful deposit of " ++ show amount

depositSTM :: Client -> Int -> STM ()
depositSTM client amount = modifyTVar (balance client) (+ amount)

withdraw :: Client -> IO ()
withdraw client = do
  printClientMessage client "How much would you like to withdraw?"
  amount <- getAmount (clientHandle client)
  runSTM
    (clientHandle client)
    (withdrawWithError client amount)
    (const $ "Successful withdraw of " ++ show amount)

withdrawWithError :: Client -> Int -> STMReturnType ()
withdrawWithError = withdrawSTM False

withdrawWithRetry :: Client -> Int -> STMReturnType ()
withdrawWithRetry = withdrawSTM True

withdrawSTM ::
     Bool -- retries if true, throws an error if false
  -> Client
  -> Int
  -> STMReturnType ()
withdrawSTM retryIfInsufficientFunds client amount = do
  currentBalance <- readTVar (balance client)
  if currentBalance < amount
    then if retryIfInsufficientFunds
           then retry
           else return $ Failure (InsufficientFunds (name client) amount)
    else do
      modifyTVar (balance client) (\current -> current - amount)
      return $ Success ()

sendMessage :: Client -> String -> IO ()
sendMessage client message = do
  clientLoggedIn <- readTVarIO (loggedIn client)
  if clientLoggedIn
    then printClientMessage client message
    else do
      currentTime <- getCurrentTime
      atomically $
        writeTQueue (messages client) $ show currentTime ++ ": " ++ message

printAllMessages :: Client -> IO ()
printAllMessages client = do
  allMessages <- atomically $ readAllMessages client
  if null allMessages
    then printClientMessage
           client
           "You have received no messages since your previous log in"
    else do
      let messagesToPrint = intercalate "\n" allMessages
      printClientMessage
        client
        "You received the following messages since your previous log in:"
      printClientMessage client messagesToPrint

readAllMessages :: Client -> STM [String]
readAllMessages client = flushTQueue (messages client)

showBalance :: Client -> IO ()
showBalance client = do
  bal <- readTVarIO (balance client)
  printClientMessage client $ "Current balance is " ++ show bal

printClientMessage :: Client -> String -> IO ()
printClientMessage client = hPutStrLn (clientHandle client)

runSTM :: Handle -> STMReturnType a -> (a -> String) -> IO ()
runSTM handle stm logMessage = do
  errorOrSuccess <- atomically stm
  case errorOrSuccess of
    Success output -> hPutStrLn handle $ logMessage output
    Failure txnError -> hPrint handle txnError

type STMReturnType a = STM (Validation TransactionError a)

data TransactionError
  = ClientDoesNotExist ClientName
  | InsufficientFunds ClientName Int
  | ClientAlreadyExists ClientName
  | CombinedTransactionError (NE.NonEmpty TransactionError)

instance Show TransactionError where
  show (ClientDoesNotExist clientName) =
    "Failed: " ++ clientName ++ " is not an existing client."
  show (InsufficientFunds clientName amount) =
    "Failed: " ++
    clientName ++
    " does not have sufficient funds to withdraw " ++ show amount ++ "."
  show (ClientAlreadyExists clientName) =
    "Failed: " ++ clientName ++ " is an existing client."
  show (CombinedTransactionError errors) =
    concatMap (\e -> show e ++ "\n") errors

instance Semigroup TransactionError where
  CombinedTransactionError xErrors <> CombinedTransactionError yErrors =
    CombinedTransactionError (xErrors <> yErrors)
  CombinedTransactionError xErrors <> yError =
    CombinedTransactionError (xErrors <> NE.fromList [yError])
  xError <> CombinedTransactionError yErrors =
    CombinedTransactionError (xError NE.<| yErrors)
  xError <> yError = CombinedTransactionError $ NE.fromList [xError, yError]
