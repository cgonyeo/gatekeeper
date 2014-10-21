module Gatekeeper.Heartbeat ( sendHeartbeats
                            ) where

import Control.Concurrent
import System.Random

import Gatekeeper.CRDT
import Gatekeeper.NetUtils
import Gatekeeper.Protobufs

second :: Int
second = 1000000

sendHeartbeats :: MVar State -> IO ()
sendHeartbeats d = do sleepamt <- randomRIO (second * 10, second * 30)
                      threadDelay sleepamt
                      (State _ c v (NetState (Host _ uid _) p)) <- readMVar d
                      putStrLn $ "Sending heartbeats to " ++ (show $ (length $ currentHosts c) - 1) ++ " hosts"
                      mapM_ (\(Host h _ _) -> forkIO $ sendMsg h p $ newMsg [] [] [] [] v) $ currentHosts c
                      sendHeartbeats d
