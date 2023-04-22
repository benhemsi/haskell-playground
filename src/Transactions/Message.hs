module Transactions.Message where

import Data.Time.Clock

data Message =
  TransferReceived UTCTime String Int

instance Show Message where
  show (TransferReceived time client amount) =
    show time ++ ": Received " ++ show amount ++ " from " ++ client
