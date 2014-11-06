module Gatekeeper.Heartbeat ( sendHeartbeats
                            ) where

import Control.Concurrent
import System.Random

import Gatekeeper.CRDT
import Gatekeeper.NetUtils
import Gatekeeper.Protobufs

minute :: Double
minute = 1000000.0 * 60.0

sendHeartbeats :: MVar State -> Double -> Double -> IO ()
sendHeartbeats d uu ul = do
        sleepamt <- randomRIO (truncate $ minute * uu, truncate $ minute * ul)
        threadDelay sleepamt
        (State _ c v (NetState (Host _ uid _) p _)) <- readMVar d
        putStrLn $ "Sending heartbeats to " ++ (show $ (length $ currentHosts c) - 1) ++ " hosts"
        mapM_ (\(Host h _ _) -> forkIO $ sendMsg h p $ newMsg [] [] [] [] v) $ currentHosts c
        sendHeartbeats d uu ul
