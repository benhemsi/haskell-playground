module Transactions.TransactionsSpec where

import Test.Hspec

spec = do
  describe "" $ do
    it "test-desctiption" $ do
      let actual = 1
          expected = 1
      actual `shouldBe` expected
