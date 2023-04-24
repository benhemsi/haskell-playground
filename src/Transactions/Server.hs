module Transactions.Server where

import Control.Concurrent.STM
import Data.Map
import Transactions.Client

data Server =
  Server
    { clients :: TVar (Map ClientName Client)
    , admin :: Client
    }
