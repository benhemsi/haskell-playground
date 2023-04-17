module Transactions.Client where

import Control.Concurrent.STM
import System.IO

data Client =
  Client
    { name :: String
    , clientHandle :: Handle
    , balance :: TVar Int
    , password :: Int
    , loggedIn :: TVar Bool
    , messages :: TQueue String
    }

type ClientName = String
