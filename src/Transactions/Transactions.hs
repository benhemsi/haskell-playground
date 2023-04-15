{-# LANGUAGE OverloadedStrings #-}

module Transactions.Transactions where

import Control.Concurrent.STM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Validation
import Network.Run.TCP
import Network.Socket
import System.IO

transactionsMain :: IO ()
transactionsMain = do
  server <- newServer
  runTCPServer (Just "localhost") (show port) $ \sock -> do
    handle <- socketToHandle sock ReadWriteMode
    talk handle server

port :: PortNumber
port = 44444

type ClientName = String

newtype Client =
  Client
    { balance :: TVar Int
    }

newtype Server =
  Server
    { clients :: TVar (Map.Map ClientName Client)
    }

talk :: Handle -> Server -> IO ()
talk handle server = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
  where
    readName = do
      hPutStrLn handle "What is your name?"
      name <- hGetLine handle
      if null name
        then readName
        else addClient handle server name 100

newServer :: IO Server
newServer =
  atomically $ do
    emptyClients <- newTVar Map.empty
    return $ Server emptyClients

addClient :: Handle -> Server -> ClientName -> Int -> IO ()
addClient handle server newClient initialBalance =
  runSTM handle $ addClientSTM server newClient initialBalance

addClientSTM :: Server -> ClientName -> Int -> STMReturnType String
addClientSTM server newClient initialBalance = do
  newBalance <- newTVar initialBalance
  currentClients <- readTVar (clients server)
  if Map.member newClient currentClients
    then return $ Failure (ClientAlreadyExists newClient)
    else do
      modifyTVar (clients server) (Map.insert newClient (Client newBalance))
      return $ Success $ newClient ++ " successfully added"

removeClient :: Handle -> Server -> ClientName -> IO ()
removeClient handle server newClient =
  runSTM handle $ removeClientSTM server newClient

removeClientSTM :: Server -> ClientName -> STMReturnType String
removeClientSTM server clientToRemove = do
  currentClients <- readTVar (clients server)
  if Map.notMember clientToRemove currentClients
    then return $ Failure (ClientDoesNotExist clientToRemove)
    else do
      modifyTVar (clients server) (Map.delete clientToRemove)
      return $ Success $ clientToRemove ++ " successfully deleted"

payDebt :: Handle -> Server -> ClientName -> ClientName -> Int -> IO ()
payDebt handle server startClient endClient amount = do
  maybeRemainder <-
    atomically $ transferReturningRemainder server startClient endClient amount
  case maybeRemainder of
    Failure errorMessage -> hPrint handle errorMessage
    Success Nothing -> hPutStrLn handle "Successful transaction"
    Success (Just remainder) -> do
      hPutStrLn
        handle
        ("Insufficient funds to make complete transfer. The account has been emptied and the remaining balance " ++
         show remainder ++ " will be taken where there are sufficient funds")
      transferWithRetry handle server startClient endClient remainder

transferWithError ::
     Handle -> Server -> ClientName -> ClientName -> Int -> IO ()
transferWithError handle server startClient endClient amount =
  runSTM handle $ transferSTM False server startClient endClient amount

transferWithRetry ::
     Handle -> Server -> ClientName -> ClientName -> Int -> IO ()
transferWithRetry handle server startClient endClient amount =
  runSTM handle $ transferSTM True server startClient endClient amount

transferReturningRemainder ::
     Server -> ClientName -> ClientName -> Int -> STMReturnType (Maybe Int)
transferReturningRemainder server startClient endClient amount = do
  _ <- depositSTM server endClient amount
  validatedBalance <- showBalanceSTM server startClient
  case validatedBalance of
    Failure errorMessage -> return $ Failure errorMessage
    Success currentBalance ->
      if currentBalance >= amount
        then do
          _ <- transferSTM False server startClient endClient amount
          return $ Success Nothing
        else do
          _ <- transferSTM False server startClient endClient currentBalance
          return $ Success (Just (amount - currentBalance))

transferSTM ::
     Bool -- retry transaction
  -> Server
  -> ClientName
  -> ClientName
  -> Int
  -> STMReturnType String
transferSTM retryIfInsufficientFunds server startClient endClient amount = do
  validatedDeposit <- depositSTM server endClient amount
  validatedWithdraw <-
    withdrawSTM retryIfInsufficientFunds server startClient amount
  return $
    Success
      ("Successful transaction from " ++ startClient ++ " to " ++ endClient) <*
    validatedDeposit <*
    validatedWithdraw

deposit :: Handle -> Server -> ClientName -> Int -> IO ()
deposit handle server client amount =
  runSTM handle $ depositSTM server client amount

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
  runSTM handle $ withdrawWithError server client amount

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
          modifyTVar (balance clientToUpdate) (amount -)
          return $ Success "Successful withdraw"

showBalance :: Server -> ClientName -> IO ()
showBalance server client = do
  currentBalance <- atomically $ showBalanceSTM server client
  putStrLn $ "Current balance is " ++ show currentBalance

showBalanceSTM :: Server -> ClientName -> STMReturnType Int
showBalanceSTM server client = do
  currentClients <- readTVar (clients server)
  case Map.lookup client currentClients of
    Nothing -> return (Failure (ClientDoesNotExist client))
    Just clientToUpdate -> do
      currentBalance <- readTVar (balance clientToUpdate)
      return $ Success currentBalance

runSTM :: Show a => Handle -> STMReturnType a -> IO ()
runSTM handle stm = do
  errorOrSuccess <- atomically stm
  case errorOrSuccess of
    Success successMessage -> hPrint handle successMessage
    Failure txnError -> hPrint handle txnError

type STMReturnType a = STM (Validation TransactionError a)

data TransactionError
  = ClientDoesNotExist ClientName
  | InsufficientFunds ClientName Int
  | ClientAlreadyExists ClientName
  | CombinedTransactionError (NE.NonEmpty TransactionError)

instance Show TransactionError where
  show (ClientDoesNotExist clientName) =
    "Transaction failed because " ++ clientName ++ " is not an existing client."
  show (InsufficientFunds clientName amount) =
    "Transaction failed because " ++
    clientName ++
    " does not have sufficient funds to withdraw " ++ show amount ++ "."
  show (ClientAlreadyExists clientName) =
    "Transaction failed because " ++ clientName ++ " is an existing client."
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
