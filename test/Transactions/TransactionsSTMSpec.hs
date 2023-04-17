module Transactions.TransactionsSTMSpec where

import Control.Concurrent.STM
import System.IO
import Test.Hspec
import Test.QuickCheck.Monadic
import Transactions.Client
import Transactions.TransactionsSTM

testHandle :: IO Handle
testHandle = openFile "test/Transactions/TransactionsSTMSpec.hs" ReadMode

spec = do
  describe "transfer" $ do
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
