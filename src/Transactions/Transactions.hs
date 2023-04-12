module Transactions.Transactions where

import Control.Concurrent.STM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Validation

transactionsMain :: IO ()
transactionsMain = putStrLn "hello"

type ClientName = String

newtype Client =
  Client
    { balance :: TVar Int
    }

newtype Server =
  Server
    { clients :: TVar (Map.Map ClientName Client)
    }

newServer :: IO Server
newServer =
  atomically $ do
    emptyClients <- newTVar Map.empty
    return $ Server emptyClients

addClient :: Server -> ClientName -> Int -> IO ()
addClient server newClient initialBalance =
  runSTM $ addClientSTM server newClient initialBalance

addClientSTM :: Server -> ClientName -> Int -> STMReturnType String
addClientSTM server newClient initialBalance = do
  newBalance <- newTVar initialBalance
  currentClients <- readTVar (clients server)
  if Map.member newClient currentClients
    then return $ Failure (ClientAlreadyExists newClient)
    else do
      modifyTVar (clients server) (Map.insert newClient (Client newBalance))
      return $ Success $ newClient ++ " successfully added"

removeClient :: Server -> ClientName -> IO ()
removeClient server newClient = runSTM $ removeClientSTM server newClient

removeClientSTM :: Server -> ClientName -> STMReturnType String
removeClientSTM server clientToRemove = do
  currentClients <- readTVar (clients server)
  if Map.notMember clientToRemove currentClients
    then return $ Failure (ClientDoesNotExist clientToRemove)
    else do
      modifyTVar (clients server) (Map.delete clientToRemove)
      return $ Success $ clientToRemove ++ " successfully deleted"

payDebt :: Server -> ClientName -> ClientName -> Int -> IO ()
payDebt server startClient endClient amount = do
  maybeRemainder <-
    atomically $ transferReturningRemainder server startClient endClient amount
  case maybeRemainder of
    Failure errorMessage -> print errorMessage
    Success Nothing -> putStrLn "Successful transaction"
    Success (Just remainder) -> do
      putStrLn
        ("Insufficient funds to make complete transfer. The account has been emptied and the remaining balance " ++
         show remainder ++ " will be taken where there are sufficient funds")
      transferWithRetry server startClient endClient remainder

transferWithError :: Server -> ClientName -> ClientName -> Int -> IO ()
transferWithError server startClient endClient amount =
  runSTM $ transferSTM False server startClient endClient amount

transferWithRetry :: Server -> ClientName -> ClientName -> Int -> IO ()
transferWithRetry server startClient endClient amount =
  runSTM $ transferSTM True server startClient endClient amount

transferReturningRemainder ::
     Server -> ClientName -> ClientName -> Int -> STMReturnType (Maybe Int)
transferReturningRemainder server startClient endClient amount = do
  depositSTM server endClient amount
  validatedBalance <- showBalanceSTM server startClient
  case validatedBalance of
    Failure errorMessage -> return $ Failure errorMessage
    Success currentBalance ->
      if currentBalance >= amount
        then do
          transferSTM False server startClient endClient amount
          return $ Success Nothing
        else do
          transferSTM False server startClient endClient currentBalance
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

deposit :: Server -> ClientName -> Int -> IO ()
deposit server client amount = runSTM $ depositSTM server client amount

depositSTM :: Server -> ClientName -> Int -> STMReturnType String
depositSTM server client amount = do
  currentClients <- readTVar (clients server)
  case Map.lookup client currentClients of
    Nothing -> return (Failure (ClientDoesNotExist client))
    Just clientToUpdate -> do
      modifyTVar (balance clientToUpdate) (+ amount)
      return $ Success "Successful deposit"

withdraw :: Server -> ClientName -> Int -> IO ()
withdraw server client amount = runSTM $ withdrawWithError server client amount

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

runSTM :: Show a => STMReturnType a -> IO ()
runSTM stm = do
  errorOrSuccess <- atomically stm
  case errorOrSuccess of
    Success successMessage -> print successMessage
    Failure txnError -> print txnError

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
