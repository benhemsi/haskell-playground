{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Transactions.Transactions where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map

transactionsMain :: IO ()
transactionsMain = putStrLn "hello"

type ClientName = String

newtype Client =
  Client
    { balance :: TVar Int
    }

newtype Server =
  Server
    { clients :: TVar (Map ClientName Client)
    }

addClient :: Server -> ClientName -> Int -> IO ()
addClient server newClient initialBalance =
  runSTM $ addClientSTM server newClient initialBalance

addClientSTM :: Server -> ClientName -> Int -> STMReturnType
addClientSTM server newClient initialBalance = do
  newBalance <- newTVar initialBalance
  currentClients <- readTVar (clients server)
  output <-
    if Map.member newClient currentClients
      then return $ Left (ClientAlreadyExists newClient)
      else do
        modifyTVar (clients server) (Map.insert newClient (Client newBalance))
        return $ Right ("Successful addition")
  return output

removeClient :: Server -> ClientName -> IO ()
removeClient server newClient = runSTM $ removeClientSTM server newClient

removeClientSTM :: Server -> ClientName -> STMReturnType
removeClientSTM server clientToRemove = do
  currentClients <- readTVar (clients server)
  output <-
    if Map.notMember clientToRemove currentClients
      then return $ Left (ClientDoesNotExist clientToRemove)
      else do
        modifyTVar (clients server) (Map.delete clientToRemove)
        return $ Right ("Successful deletion")
  return output

transfer :: Server -> ClientName -> ClientName -> Int -> IO ()
transfer server startClient endClient amount =
  runSTM $ transferSTM server startClient endClient amount

transferSTM :: Server -> ClientName -> ClientName -> Int -> STMReturnType
transferSTM server startClient endClient amount = do
  depositSTM server endClient amount
  withdrawSTM server startClient amount

deposit :: Server -> ClientName -> Int -> IO ()
deposit server client amount = runSTM $ depositSTM server client amount

depositSTM :: Server -> ClientName -> Int -> STMReturnType
depositSTM server client amount = do
  currentClients <- readTVar (clients server)
  output <-
    case Map.lookup client currentClients of
      Nothing -> return (Left (ClientDoesNotExist client))
      Just clientToUpdate -> do
        modifyTVar (balance clientToUpdate) (+ amount)
        return $ Right "Successful deposit."
  return output

withdraw :: Server -> ClientName -> Int -> IO ()
withdraw server client amount = runSTM $ withdrawSTM server client amount

withdrawSTM :: Server -> ClientName -> Int -> STMReturnType
withdrawSTM server client amount = depositSTM server client (-amount)

showBalance :: Server -> ClientName -> IO ()
showBalance = undefined

runSTM :: STMReturnType -> IO ()
runSTM stm = do
  errorOrSuccess <- atomically stm
  case errorOrSuccess of
    Right successMessage -> putStrLn successMessage
    Left txnError -> putStrLn (show txnError)

type STMReturnType = STM (Either TransactionError String)

data TransactionError
  = ClientDoesNotExist ClientName
  | InsufficientFunds ClientName
  | ClientAlreadyExists ClientName

instance Show TransactionError where
  show (ClientDoesNotExist clientName) =
    "Transaction failed because " ++ clientName ++ " is not an existing client."
  show (InsufficientFunds clientName) =
    "Transaction failed because " ++
    clientName ++ " does not have sufficient funds to withdraw that amount."
  show (ClientAlreadyExists clientName) =
    "Transaction failed because " ++ clientName ++ " is an existing client."
