module Gatekeeper.Heartbeat ( sendHeartbeats
                            ) where

import Control.Concurrent
import System.Random

import Gatekeeper.CRDT
import Gatekeeper.NetUtils
import Gatekeeper.Protobufs

second :: Double
second = 1000000

sendHeartbeats :: MVar State -> IO ()
sendHeartbeats d = do (State _ _ _ (NetState _ _ _ uu ul)) <- readMVar d
                      sleepamt <- randomRIO (truncate $ second * 10 * uu, truncate $ second * 30 * ul)
                      threadDelay sleepamt
                      (State _ c v (NetState (Host _ uid _) p _ _ _)) <- readMVar d
                      putStrLn $ "Sending heartbeats to " ++ (show $ (length $ currentHosts c) - 1) ++ " hosts"
                      mapM_ (\(Host h _ _) -> forkIO $ sendMsg h p $ newMsg [] [] [] [] v) $ currentHosts c
                      sendHeartbeats d
