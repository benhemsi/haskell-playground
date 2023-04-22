{-# LANGUAGE OverloadedStrings #-}

module Transactions.TransactionsIO where

import Control.Concurrent.STM
import Data.Hashable
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Validation
import Network.Run.TCP
import Network.Socket
import System.IO
import Text.Read
import Transactions.Client
import Transactions.Message
import Transactions.Server
import Transactions.TransactionError
import Transactions.TransactionsSTM

transactionsMain :: IO ()
transactionsMain = do
  server <- newServer
  runTCPServer (Just "localhost") (show port) $ \sock -> do
    handle <- socketToHandle sock ReadWriteMode
    talk handle server

port :: PortNumber
port = 44444

data StartAction
  = SignUp
  | LogIn
  deriving (Show, Read, Enum, Bounded)

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
  client <- startAction handle server
  clientAction server client

startAction :: Handle -> Server -> IO Client
startAction handle server = do
  let options :: [StartAction]
      options = [minBound .. maxBound]
  hPutStrLn handle $
    "Select one of the following: " ++ intercalate ", " (map show options)
  response <- hGetLine handle
  case readMaybe response of
    Nothing -> do
      hPutStrLn handle "Invalid response. Try again"
      startAction handle server
    Just action -> do
      runStartAction handle server action

runStartAction :: Handle -> Server -> StartAction -> IO Client
runStartAction handle server action =
  case action of
    SignUp -> addClient handle server
    LogIn -> logIn handle server

clientAction :: Server -> Client -> IO ()
clientAction server client = do
  let options :: [ClientAction]
      options = [minBound .. maxBound]
  printClientMessage client $
    "Select one of the following: " ++ intercalate ", " (map show options)
  response <- clientGetLine client
  case readMaybe response of
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

logIn :: Handle -> Server -> IO Client
logIn handle server = do
  client <- getClient handle server
  hPutStrLn handle "Enter password:"
  enterPassword client
  where
    enterPassword client = do
      psw <- hGetLine handle
      if hash psw == password client
        then do
          hPutStrLn handle "Successful login"
          _ <-
            atomically $ do
              _ <- writeTVar (loggedIn client) True
              putTMVar (clientHandle client) handle
          printAllMessages client
          return client
        else do
          printClientMessage client "Incorrect passord. Try again"
          enterPassword client

logOut :: Server -> Client -> IO ()
logOut server client = do
  printClientMessage client "Successful log out"
  handle <-
    atomically $ do
      writeTVar (loggedIn client) False
      takeTMVar (clientHandle client)
  talk handle server

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

removeClient :: Handle -> Server -> ClientName -> IO ()
removeClient handle server newClient =
  runSTM
    handle
    (removeClientSTM server newClient)
    (\_ -> newClient ++ " successfully deleted")

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
  handle <- atomically $ readTMVar (clientHandle startClient)
  endClient <- getClient handle server
  printClientMessage startClient "How much would you like to send?"
  amount <- getAmount handle
  maybeTransfer <- atomically (transferSTM False startClient endClient amount)
  case maybeTransfer of
    Success _ -> do
      printClientMessage startClient $
        "Successfully sent money to " ++ name endClient
      currentTime <- getCurrentTime
      sendMessage endClient $
        TransferReceived currentTime (name startClient) amount
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
deposit :: Client -> IO ()
deposit client = do
  printClientMessage client "How much would you like to deposit?"
  handle <- atomically $ readTMVar (clientHandle client)
  amount <- getAmount handle
  _ <- atomically $ depositSTM client amount
  printClientMessage client $ "Successful deposit of " ++ show amount

withdraw :: Client -> IO ()
withdraw client = do
  printClientMessage client "How much would you like to withdraw?"
  handle <- atomically $ readTMVar (clientHandle client)
  amount <- getAmount handle
  runSTM
    handle
    (withdrawWithError client amount)
    (const $ "Successful withdraw of " ++ show amount)

sendMessage :: Client -> Message -> IO ()
sendMessage client message = do
  clientLoggedIn <- readTVarIO (loggedIn client)
  if clientLoggedIn
    then printClientMessage client (show message)
    else do
      atomically $ writeTQueue (messages client) message

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

showBalance :: Client -> IO ()
showBalance client = do
  bal <- readTVarIO (balance client)
  printClientMessage client $ "Current balance is " ++ show bal

printClientMessage :: Client -> String -> IO ()
printClientMessage client message = do
  handle <- atomically $ readTMVar (clientHandle client)
  hPutStrLn handle message

clientGetLine :: Client -> IO String
clientGetLine client = do
  handle <- atomically $ readTMVar (clientHandle client)
  hGetLine handle

runSTM :: Handle -> STMReturnType a -> (a -> String) -> IO ()
runSTM handle stm logMessage = do
  errorOrSuccess <- atomically stm
  case errorOrSuccess of
    Success output -> hPutStrLn handle $ logMessage output
    Failure txnError -> hPrint handle txnError
