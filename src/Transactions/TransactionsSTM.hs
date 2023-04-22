module Transactions.TransactionsSTM where

import Control.Concurrent.STM
import Data.Hashable
import qualified Data.Map as Map
import Data.Validation
import System.IO
import Transactions.Client
import Transactions.Server
import Transactions.TransactionError

type STMReturnType a = STM (Validation TransactionError a)

getClientSTM :: Server -> ClientName -> STMReturnType Client
getClientSTM server clientName = do
  currentClients <- readTVar (clients server)
  case Map.lookup clientName currentClients of
    Just client -> return $ Success client
    Nothing -> return $ Failure (ClientDoesNotExist clientName)

buildClientSTM :: Maybe Handle -> ClientName -> String -> Int -> STM Client
buildClientSTM maybeHandle clientName psw initialBalance = do
  newBalance <- newTVar initialBalance
  loggedInBool <- newTVar True
  emptyMessages <- newTQueue
  handleTVar <- maybe newEmptyTMVar newTMVar maybeHandle
  return $
    Client
      clientName
      handleTVar
      newBalance
      (hash psw)
      loggedInBool
      emptyMessages

-- requires that the client does not already exist
addClientSTM :: Handle -> Server -> ClientName -> String -> Int -> STM Client
addClientSTM handle server clientName psw initialBalance = do
  newClient <- buildClientSTM (Just handle) clientName psw initialBalance
  modifyTVar (clients server) (Map.insert clientName newClient)
  return newClient

depositSTM :: Client -> Int -> STM ()
depositSTM client amount = modifyTVar (balance client) (+ amount)

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

transferSTM ::
     Bool -- retry transaction
  -> Client
  -> Client
  -> Int
  -> STMReturnType ()
transferSTM retryIfInsufficientFunds startClient endClient amount = do
  validatedWithdraw <- withdrawSTM retryIfInsufficientFunds startClient amount
  case validatedWithdraw of
    Success _ -> Success <$> depositSTM endClient amount
    failure -> return failure

readAllMessages :: Client -> STM [String]
readAllMessages client = do
  msgs <- flushTQueue (messages client)
  return $ map show msgs

removeClientSTM :: Server -> ClientName -> STMReturnType ()
removeClientSTM server clientToRemove = do
  currentClients <- readTVar (clients server)
  if Map.notMember clientToRemove currentClients
    then return $ Failure (ClientDoesNotExist clientToRemove)
    else do
      modifyTVar (clients server) (Map.delete clientToRemove)
      return $ Success ()
