module Transactions.Client where

import Control.Concurrent.STM
import System.IO
import Transactions.Message

data Client =
  Client
    { name :: String
    , clientHandle :: TMVar Handle
    , balance :: TVar Int
    , password :: Int
    , loggedIn :: TVar Bool
    , messages :: TQueue Message
    }

type ClientName = String
