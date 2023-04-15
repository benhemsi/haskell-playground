{-# LANGUAGE OverloadedStrings #-}

module Transactions.Transactions where

import Control.Concurrent.STM
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

talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  client <- start
  takeRequest client
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
    takeRequest client = do
      hPutStrLn handle "Select 'transfer' or 'showBalance'"
      action <- hGetLine handle
      if action == "transfer"
        then do
          transferWithError handle server client
          takeRequest client
        else if action == "showBalance"
               then do
                 showBalance handle client
                 takeRequest client
               else do
                 hPutStrLn handle $ action ++ " is invalid. Choose again"
                 takeRequest client

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
  initialBalance <- enterDeposit
  client <- atomically (addClientSTM server clientName psw initialBalance)
  hPutStrLn handle $ clientName ++ "successfully added"
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
    enterDeposit = do
      initialBalanceStr <- hGetLine handle
      let initialBalance = readMaybe initialBalanceStr :: Maybe Int
      case initialBalance of
        Just bal ->
          if bal > 0
            then return bal
            else do
              hPutStrLn handle "Balance must be greater than zero. Try again"
              enterDeposit
        Nothing -> do
          hPutStrLn handle "Invalid number. Try again"
          enterDeposit

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
          return client
        else do
          hPutStrLn handle "Incorrect passord. Try again"
          enterPassword client

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
  endClient <- hGetLine handle
  hPutStrLn handle "How much would you like to send?"
  amountStr <- hGetLine handle
  let amount = read amountStr :: Int
  runSTM
    handle
    (transferSTM False server (name startClient) endClient amount)
    (\_ ->
       "Successful transaction from " ++ name startClient ++ " to " ++ endClient)

transferWithRetry ::
     Handle -> Server -> ClientName -> ClientName -> Int -> IO ()
transferWithRetry handle server startClient endClient amount =
  runSTM
    handle
    (transferSTM True server startClient endClient amount)
    (\_ -> "Successful transaction from " ++ startClient ++ " to " ++ endClient)

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
  -> Server
  -> ClientName
  -> ClientName
  -> Int
  -> STMReturnType ()
transferSTM retryIfInsufficientFunds server startClient endClient amount = do
  validatedDeposit <- depositSTM server endClient amount
  validatedWithdraw <-
    withdrawSTM retryIfInsufficientFunds server startClient amount
  return $ Success () <* validatedDeposit <* validatedWithdraw

deposit :: Handle -> Server -> ClientName -> Int -> IO ()
deposit handle server client amount =
  runSTM handle (depositSTM server client amount) (const "Successful deposit")

depositSTM :: Server -> ClientName -> Int -> STMReturnType String
depositSTM server client amount = do
  currentClients <- readTVar (clients server)
  case Map.lookup client currentClients of
    Nothing -> return (Failure (ClientDoesNotExist client))
    Just clientToUpdate -> do
      modifyTVar (balance clientToUpdate) (+ amount)
      return $ Success "Successful deposit"

withdraw :: Handle -> Server -> ClientName -> Int -> IO ()
withdraw handle server client amount =
  runSTM
    handle
    (withdrawWithError server client amount)
    (const "Successful withdraw")

withdrawWithError :: Server -> ClientName -> Int -> STMReturnType String
withdrawWithError = withdrawSTM False

withdrawWithRetry :: Server -> ClientName -> Int -> STMReturnType String
withdrawWithRetry = withdrawSTM True

withdrawSTM ::
     Bool -- retries if true, throws an error if false
  -> Server
  -> ClientName
  -> Int
  -> STMReturnType String
withdrawSTM retryIfInsufficientFunds server client amount = do
  currentClients <- readTVar (clients server)
  case Map.lookup client currentClients of
    Nothing -> return (Failure (ClientDoesNotExist client))
    Just clientToUpdate -> do
      currentBalance <- readTVar (balance clientToUpdate)
      if currentBalance < amount
        then if retryIfInsufficientFunds
               then retry
               else return $ Failure (InsufficientFunds client amount)
        else do
          modifyTVar (balance clientToUpdate) (\current -> current - amount)
          return $ Success "Successful withdraw"

showBalance :: Handle -> Client -> IO ()
showBalance handle client = do
  bal <- readTVarIO (balance client)
  hPutStr handle $ "Current balance is " ++ show bal

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
