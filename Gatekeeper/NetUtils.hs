module Gatekeeper.NetUtils ( lookupHost
                           , sendState
                           , askForNodes
                           , sendTagDeltas
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
                      Left e -> do putStrLn $ "Error fetching host IP: " ++ (show e)
                                   return s
                      Right ips -> if (length ips) > 0
                                       then return (show $ ips !! 0)
                                       else return s

sendState :: MVar State -> String  -> IO ()
sendState d t = do
                (State a c (NetState h p v m)) <- readMVar d
                sendMsg t p $ stateToMsg (State a c (NetState h p v m))

askForNodes :: String -> NS.ServiceName -> IO ()
askForNodes l p = sendMsg l p (newMsg [] [] [] [] [])

sendTagDeltas :: MVar State -> [Tag] -> [Tag] -> IO ()
sendTagDeltas d toadd todel = 
        do (State s (Cluster a r) (NetState (Host h uid) p v m)) <- takeMVar d
           let newclocks = incClock v uid
           let hosts = filter (\hst -> not (hst `elem` r)) a
           mapM_ (\(Host h u) -> if u /= uid
                                     then forkIO $ sendMsg h p (newMsg toadd todel [] [] newclocks)
                                     else myThreadId) hosts
           putMVar d (State s (Cluster a r) (NetState (Host h uid) p newclocks m))

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
        (State s (Cluster a r) (NetState (Host h _) p v m)) <- takeMVar d
        let myhost = (Host h (show uid))
        let myvclk = (HostClock (show uid) 1)
        let hosts = filter (\h -> not (h `elem` r)) a
        mapM_ (\(Host hst _) -> forkIO $ sendMsg hst p (newMsg [] [] [myhost] [] [myvclk])) hosts
        let newstate = (State s (Cluster a r) (NetState myhost p (myvclk:v) m))
        putMVar d newstate
