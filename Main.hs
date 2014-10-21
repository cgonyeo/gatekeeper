{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Network
import System.Environment
import System.Exit
import System.IO

import qualified Data.UUID.V1 as U1

import Gatekeeper.CRDT
import Gatekeeper.Heartbeat
import Gatekeeper.LDAP
import Gatekeeper.Network
import Gatekeeper.NetUtils

main :: IO ()
main = do
        progName <- getProgName
        args <- getArgs
        if (length args) >= 2 && (length args <= 3)
            then start progName args
            else do putStrLn $ "Usage: " ++ progName ++ " <hostname> <portnum> (<known host>)"
                    exitFailure

getInitData :: IO (MVar State)
getInitData = do -- May eventually read from a config file or some shit
       m <- newEmptyMVar
       putMVar m (State (Set [] []) (Cluster [] []) [] (NetState (Host "" "" (HostClock "" 0)) "0"))
       return m

start :: String -> [String] -> IO ()
start progName args = do d <- getInitData
                         host <- lookupHost hosttext
                         (Just uid) <- U1.nextUUID
                         modifyMVar_ d (\(State s (Cluster a r) v (NetState _ _)) 
                                          -> let u = (show uid)
                                                 newhost = (Host host u (HostClock u 0))
                                                 vclock = (HostClock u 0)
                                             in return $ State s (Cluster (newhost:a) r) (vclock:v) (NetState newhost port))
                         putStrLn $ progName ++ " started on port '" ++ port ++ "'. I am host '" ++ host ++ "'."
                         forkIO $ netloop d =<< listenOn portNum
                         forkIO $ ldaploop d
                         if (length args) == 3
                             then do forkIO $ addSelf d (args !! 2) port
                                     return ()
                             else return ()
                         sendHeartbeats d
                         return ()
                         where port = (args !! 1)
                               hosttext = (args !! 0)
                               portNum = PortNumber $ fromIntegral $ (read port :: Int)
