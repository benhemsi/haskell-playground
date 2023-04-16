{-# LANGUAGE OverloadedStrings #-}

module Transactions.Transactions where

import Control.Concurrent.STM
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
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
    , balance :: TVar Int
    , password :: String
    , loggedIn :: TVar Bool
    }

newtype Server =
  Server
    { clients :: TVar (Map.Map ClientName Client)
    }

data ClientAction
  = Transfer
  | ShowBalance
  | LogOut
  deriving (Show, Read, Enum, Bounded)

talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  client <- start
  clientAction handle server client
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

clientAction :: Handle -> Server -> Client -> IO ()
clientAction handle server client = do
  let options :: [ClientAction]
      options = [minBound .. maxBound]
  hPutStrLn handle $
    "Select one of the following: " ++ intercalate ", " (map show options)
  response <- hGetLine handle
  let maybeAction = readMaybe response
  case maybeAction of
    Nothing -> do
      hPutStrLn handle "Invalid response. Try again"
      clientAction handle server client
    Just action -> do
      runClientAction handle server client action
      clientAction handle server client

runClientAction :: Handle -> Server -> Client -> ClientAction -> IO ()
runClientAction handle server client action =
  case action of
    Transfer -> transferWithError handle server client
    ShowBalance -> showBalance handle client
    LogOut -> logOut handle server client

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
  client <- atomically (addClientSTM server clientName psw initialBalance)
  hPutStrLn handle $ clientName ++ " successfully added"
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
addClientSTM :: Server -> ClientName -> String -> Int -> STM Client
addClientSTM server clientName psw initialBalance = do
  newBalance <- newTVar initialBalance
  loggedInBool <- newTVar True
  let newClient = Client clientName newBalance psw loggedInBool
  modifyTVar (clients server) (Map.insert clientName newClient)
  return newClient

logIn :: Handle -> Server -> IO Client
logIn handle server = do
  client <- getClient handle server
  hPutStrLn handle "Enter password:"
  enterPassword client
  where
    enterPassword client = do
      psw <- hGetLine handle
      if psw == password client
        then do
          hPutStrLn handle "Successful login"
          _ <- atomically $ writeTVar (loggedIn client) True
          return client
        else do
          hPutStrLn handle "Incorrect passord. Try again"
          enterPassword client

logOut :: Handle -> Server -> Client -> IO ()
logOut handle server client = do
  _ <- atomically $ writeTVar (loggedIn client) False
  talk handle server

getAmount :: Handle -> IO Int
getAmount handle = do
  enterAmount
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
transferWithError :: Handle -> Server -> Client -> IO ()
transferWithError handle server startClient = do
  hPutStrLn handle "Who would you like to transfer with?"
  endClient <- getClient handle server
  hPutStrLn handle "How much would you like to send?"
  amount <- getAmount handle
  runSTM
    handle
    (transferSTM False startClient endClient amount)
    (\_ ->
       "Successful transaction from " ++
       name startClient ++ " to " ++ name endClient)

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

deposit :: Handle -> Client -> Int -> IO ()
deposit handle client amount = do
  _ <- atomically $ depositSTM client amount
  hPutStrLn handle $ "Successful deposit of " ++ show amount

depositSTM :: Client -> Int -> STM ()
depositSTM client amount = modifyTVar (balance client) (+ amount)

withdraw :: Handle -> Client -> Int -> IO ()
withdraw handle client amount =
  runSTM handle (withdrawWithError client amount) (const "Successful withdraw")

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

showBalance :: Handle -> Client -> IO ()
showBalance handle client = do
  bal <- readTVarIO (balance client)
  hPutStr handle $ "Current balance is " ++ show bal ++ "\n"

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
