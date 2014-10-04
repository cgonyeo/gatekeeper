{-# LANGUAGE OverloadedStrings #-}
import qualified Data.UUID.V1 as U1
import Network
import Control.Concurrent
import System.Environment
import System.IO
import System.Exit

import Gatekeeper.Network
import Gatekeeper.NetUtils
import Gatekeeper.CRDT
import Gatekeeper.LDAP

main :: IO ()
main = do
        progName <- getProgName
        args <- getArgs
        d <- getInitData
        case (length args) of
            2 -> do (Just uid) <- U1.nextUUID
                    ip <- lookupHost (args !! 0)
                    modifyMVar_ d (\(State s (Cluster a r) (NetState h p v m)) 
                                    -> let newhost = (Host ip (show uid))
                                           u = (show uid)
                                           vclock = ((HostClock u 0):v)
                                       in return $ (State s (Cluster (newhost:a) r) (NetState newhost p vclock m)))
                    modifyMVar_ d (\s -> return $ changeMembership s Member)
            3 -> do forkIO $ askForNodes (args !! 2) (args !! 1)
                    modifyMVar_ d (\s -> return $ changeMembership s Joining)
            _ -> do putStrLn $ "Usage: " ++ progName ++ " <hostname> <portnum> (<known host>)"
                    exitFailure
        forkIO $ start progName args d
        ldaploop d

getInitData :: IO (MVar State)
getInitData = do
       m <- newEmptyMVar
       putMVar m (State (Set [] []) (Cluster [] []) (NetState (Host "" "") "0" [] NotAMember))
       return m

start :: String -> [String] -> MVar State -> IO ()
start progName args d = do host <- lookupHost hosttext
                           putStrLn $ progName ++ " started on port '" ++ port ++ "'. I am host '" ++ host ++ "'."
                           modifyMVar_ d (\(State s c (NetState (Host _ u) _ v m)) 
                                            -> return $ State s c (NetState (Host host u) port v m))
                           s <- listenOn $ PortNumber $ fromIntegral $ (read port :: Int)
                           netloop s d
                           return ()
                           where port = (args !! 1)
                                 hosttext = (args !! 0)
