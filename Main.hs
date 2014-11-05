{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Network
import System.Environment
import System.Exit
import System.IO
import Data.ConfigFile
import Data.Either.Utils

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
        if (length args) == 1
            then start progName args
            else do putStrLn $ "Usage: " ++ progName ++ " <config>"
                    exitFailure

getInitData :: IO (MVar State)
getInitData = do -- May eventually read from a config file or some shit
       m <- newEmptyMVar
       putMVar m (State (Set [] []) (Cluster [] []) [] (NetState (Host "" "" (HostClock "" 0)) "0" (LdapInfo "" "" "" 0 0) 0 0))
       return m

start :: String -> [String] -> IO ()
start progName args = do cfg <- forceEither `fmap` readfile emptyCP (args !! 0)
                         d <- getInitData
                         let hosttext      = forceEither $ get cfg "DEFAULT" "myhost"
                         let port          = forceEither $ get cfg "DEFAULT" "port"
                         let ldapurl       = forceEither $ get cfg "DEFAULT" "ldapurl"
                         let ldapusername  = forceEither $ get cfg "DEFAULT" "ldapusername"
                         let ldappassword  = forceEither $ get cfg "DEFAULT" "ldappassword"
                         let updateupper   = forceEither $ get cfg "DEFAULT" "updateupper"
                         let updatelower   = forceEither $ get cfg "DEFAULT" "updatelower"
                         let hbu           = forceEither $ get cfg "DEFAULT" "heartbeatupper"
                         let hbl           = forceEither $ get cfg "DEFAULT" "heartbeatlower"
                         let knownnode     = case get cfg "DEFAULT" "knownnode" of
                                                Left _  -> Nothing
                                                Right a -> Just a
                         let portNum  = PortNumber $ fromIntegral $ (read port :: Int)
                         host <- lookupHost hosttext
                         (Just uid) <- U1.nextUUID
                         modifyMVar_ d (\(State s (Cluster a r) v _) 
                                          -> let u       = (show uid)
                                                 newhost = (Host host u (HostClock u 0))
                                                 vclock  = (HostClock u 0)
                                                 ld      = (LdapInfo ldapurl ldapusername ldappassword updateupper updatelower)
                                             in return $ State s (Cluster (newhost:a) r) (vclock:v) (NetState newhost port ld hbu hbl))
                         putStrLn $ progName ++ " started on port '" ++ port ++ "'. I am host '" ++ host ++ "'."
                         forkIO $ netloop d =<< listenOn portNum
                         forkIO $ ldaploop d
                         case knownnode of
                             Just h  -> forkIO $ addSelf d h port
                             Nothing -> myThreadId
                         sendHeartbeats d
                         return ()
