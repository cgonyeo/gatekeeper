module Gatekeeper.NetUtils ( lookupHost
                           , sendNodes
                           , askForNodes
                           , sendAddDeltas
                           , sendDelDeltas
                           , sendMsg
                           , addSelf
                           ) where

import Control.Concurrent
import Network
import qualified Network.Socket as NS
import qualified Data.UUID.V1 as U1
import qualified Data.ByteString.Char8 as C
import Network.DNS.Lookup
import Network.DNS.Resolver

import Gatekeeper.CRDT
import Gatekeeper.Protobufs

lookupHost :: String -> IO String
lookupHost s = do rs <- makeResolvSeed defaultResolvConf
                  ips <- withResolver rs $ \r -> lookupA r $ C.pack s
                  case ips of
                      Left e -> error (show e)
                      Right ips -> if (length ips) > 0
                                       then return (show $ ips !! 0)
                                       else return s

sendNodes :: MVar State -> String -> IO ()
sendNodes d c = do 
                (State _ (Cluster a r) (NetState _ p _ _)) <- readMVar d
                sendMsg c p (addHostsMsg a)
                sendMsg c p (delHostsMsg r)

askForNodes :: String -> NS.ServiceName -> IO ()
askForNodes l p = sendMsg l p reqNodesMsg

sendAddDeltas :: MVar State -> [Tag] -> IO ()
sendAddDeltas d toadd = 
        do modifyMVar_ d (\(State s c (NetState (Host h u) p v m)) 
                           -> return $ (State s c (NetState (Host h u) p (incClock v u) m)))
           (State s (Cluster a r) (NetState (Host _ uid) p _ _)) <- readMVar d
           let hosts = filter (\h -> not (h `elem` r)) a
           mapM_ (\(Host h u) -> if u /= uid
                                     then forkIO $ sendMsg h p (addTagsMsg toadd)
                                     else myThreadId) hosts

sendDelDeltas :: MVar State -> [Tag] -> IO ()
sendDelDeltas d todel = 
        do modifyMVar_ d (\(State s c (NetState (Host h u) p v m)) 
                           -> return $ (State s c (NetState (Host h u) p (incClock v u) m)))
           (State s (Cluster a r) (NetState (Host _ uid) p _ _)) <- readMVar d
           let hosts = filter (\h -> not (h `elem` r)) a
           mapM_ (\(Host h u) -> if u /= uid
                                     then forkIO $ sendMsg h p (delTagsMsg todel)
                                     else myThreadId) hosts

sendMsg :: String -> NS.ServiceName -> Msg -> IO ()
sendMsg h p m = do
        addrInfo <- NS.getAddrInfo Nothing (Just h) (Just p)
        let serverAddr = head addrInfo
        sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
        NS.connect sock (NS.addrAddress serverAddr)
        NS.send sock (msgToBlob m)
        sClose sock

addSelf :: MVar State -> IO ()
addSelf d = do
        (Just uid) <- U1.nextUUID
        (State _ (Cluster a r) (NetState (Host h _) p _ _)) <- readMVar d
        let lst = filter (\h -> not (h `elem` r)) a
        modifyMVar_ d (\(State s (Cluster a r) (NetState (Host h _) p v m)) 
                         -> let u = (show uid)
                                host = (Host h u)
                                vclock = ((HostClock u 0):v)
                            in return $ (State s (Cluster (host:a) r) (NetState host p vclock m)))
        modifyMVar_ d (\(State s c (NetState (Host h u) p v m)) 
                         -> return $ (State s c (NetState (Host h u) p (incClock v u) m)))
        mapM_ (\(Host hst _) -> forkIO $ sendMsg hst p (addHostsMsg [(Host h (show uid))])) lst
