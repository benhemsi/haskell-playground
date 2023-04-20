module Transactions.TransactionsSTMSpec where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Validation as V
import System.IO
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Transactions.Client
import Transactions.TransactionsSTM

maxBalance = 10

maxClientNumber = 10

minTxnNumber = 10

maxTxnNumber = 10

testHandle :: IO Handle
testHandle = openFile "test/Transactions/TransactionsSTMSpec.hs" ReadMode

genClient :: Int -> Handle -> Gen (IO Client)
genClient clientN handle = do
  bal <- chooseInt (0, maxBalance)
  let clientName = "test" ++ show clientN
      client = do
        atomically $ buildClientSTM handle clientName "password" bal
  return client

generateClients :: Int -> Handle -> Gen (IO (Map.Map ClientName Client))
generateClients numberOfClients handle = do
  let clients :: [IO (IO Client)]
      clients = [generate (genClient n handle) | n <- [0 .. numberOfClients]]
      flattenedClients :: [IO Client]
      flattenedClients = map join clients
      ioClients :: IO [Client]
      ioClients = sequence flattenedClients
      mapOfClients :: IO (Map.Map ClientName Client)
      mapOfClients = fmap (Map.fromList . map (\c -> (name c, c))) ioClients
  return mapOfClients

generateTransaction ::
     Int -> Map.Map ClientName Client -> Gen (STMReturnType ())
generateTransaction numberOfClients clients = do
  firstClientN <- chooseInt (0, numberOfClients)
  secondClientN <- chooseInt (0, numberOfClients)
  amount <- chooseInt (0, maxBalance)
  let firstClient = clients Map.! ("test" ++ show firstClientN)
      secondClient = clients Map.! ("test" ++ show secondClientN)
  return $ transferSTM False firstClient secondClient amount

generateTransactions ::
     Int -> Map.Map ClientName Client -> Gen [STMReturnType ()]
generateTransactions numberOfClients clients = do
  numberOfTxns <- chooseInt (minTxnNumber, maxTxnNumber)
  vectorOf numberOfTxns (generateTransaction numberOfClients clients)

readTotalClientAmount :: Map.Map ClientName Client -> IO Int
readTotalClientAmount clients = do
  let amountIOs = map (readTVarIO . balance) (Map.elems clients)
  amounts <- sequence amountIOs
  return $ sum amounts

runTxns :: [STMReturnType ()] -> IO (Int, Int)
runTxns txns = do
  let txnsIO = map atomically txns
  runTransfers <- sequence txnsIO
  let validCount = length [x | V.Success x <- runTransfers]
      invalidCount = length [e | V.Failure e <- runTransfers]
  return (validCount, invalidCount)

generateAndRunTransactions :: Int -> Handle -> IO (Int, Int)
generateAndRunTransactions numberOfClients handle = do
  clients <- join $ generate (generateClients numberOfClients handle)
  initialAmount <- readTotalClientAmount clients
  putStrLn $ "Initial amount: " ++ show initialAmount
  txns <- generate $ generateTransactions numberOfClients clients
  (validCount, invalidCount) <- runTxns txns
  putStrLn $ "Valid transfers: " ++ show validCount
  putStrLn $ "Invalid transfers: " ++ show invalidCount
  endAmount <- readTotalClientAmount clients
  putStrLn $ "End amount: " ++ show endAmount
  return (initialAmount, endAmount)

spec = do
  describe "transfer" $ do
    it "should preserve the total sum of balances when only transfers are run" $
      monadicIO $ do
        handle <- run testHandle
        clientN <- pick (chooseInt (0, maxClientNumber))
        (initalAmount, endAmount) <-
          run $ generateAndRunTransactions clientN handle
        assert (initalAmount == endAmount)
    it "should move money from one client to another" $
      monadicIO $ do
        handle <- run testHandle
        let transactionSTM = do
              client1 <- buildClientSTM handle "test1" "password" 100
              client2 <- buildClientSTM handle "test2" "password" 100
              _ <- transferSTM False client1 client2 50
              return (client1, client2)
        (client1, client2) <- run $ atomically transactionSTM
        client1Balance <- run $ readTVarIO (balance client1)
        client2Balance <- run $ readTVarIO (balance client2)
        assert (client1Balance == 50)
        assert (client2Balance == 150)
