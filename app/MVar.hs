module MVar where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO

main :: IO ()
main = deadlock

put2 :: IO ()
put2 = do
  m <- newEmptyMVar
  _ <-
    forkIO $ do
      putMVar m 'x'
      putMVar m 'y'
  r <- takeMVar m
  print r
  s <- takeMVar m
  print s

blocking :: IO ()
blocking = do
  m <- newEmptyMVar
  r <- takeMVar m
  putStrLn r

fork :: IO ()
fork = do
  hSetBuffering stdout NoBuffering
  _ <- forkIO (replicateM_ 1000 (putChar 'A'))
  replicateM_ 1000 (putChar 'B')

forkMVar :: IO ()
forkMVar = do
  hSetBuffering stdout NoBuffering
  m <- newEmptyMVar
  _ <-
    forkIO
      (replicateM_
         1000
         (do putMVar m 'A'
             r <- takeMVar m
             putChar r))
  replicateM_
    1000
    (do putMVar m 'B'
        r <- takeMVar m
        putChar r)

forkMVar2 :: IO ()
forkMVar2 = do
  hSetBuffering stdout NoBuffering
  m <- newEmptyMVar
  _ <- forkIO $ replicateM_ 1000 (putMVar m 'A')
  _ <- forkIO $ replicateM_ 1000 (putMVar m 'B')
  replicateM_
    2000
    (do r <- takeMVar m
        putChar r)

forkTMVar :: IO ()
forkTMVar = do
  hSetBuffering stdout NoBuffering
  m <- newEmptyTMVarIO
  _ <- forkIO $ replicateM_ 1000 (atomically $ putTMVar m 'A')
  _ <- forkIO $ replicateM_ 1000 (atomically $ putTMVar m 'B')
  replicateM_
    2000
    (do r <- atomically $ takeTMVar m
        putChar r)

deadlock :: IO ()
deadlock = do
  m1 <- newMVar (0 :: Int)
  m2 <- newMVar (0 :: Int)
  txns <- newMVar (0 :: Int)
  _ <-
    forkIO $
    replicateM_ 1000 $ do
      a <- takeMVar m1
      b <- takeMVar m2
      putMVar m2 (a + 1)
      -- putMVar m1 (a + 1)
      modifyMVar_ txns (\x -> pure $ x + 1)
  -- _ <-
  --   forkIO $
  replicateM_ 1000 $ do
    a <- takeMVar m2
    b <- takeMVar m1
    putMVar m1 (a - 1)
    -- putMVar m2 (a - 1)
    modifyMVar_ txns (\x -> pure $ x + 1)
  t <- takeMVar txns
  a <- takeMVar m1
  b <- takeMVar m2
  print t
  print a
  print b
