module Gatekeeper.NetUtils ( lookupHost
                           , sendState
                           , sendTagDeltas
                           , sendOperations
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
sendState d t = return ()

sendTagDeltas :: MVar State -> [Tag] -> [Tag] -> IO ()
sendTagDeltas d toadd todel = 
        do (State s c v (NetState (Host h uid (HostClock hu hc)) p ld)) <- takeMVar d
           let newtoadd = map (\(Tag n i u _) -> (Tag n i u (HostClock hu (hc+1)))) toadd
           let newtodel = map (\(Tag n i u _) -> (Tag n i u (HostClock hu (hc+1)))) todel
           let newclocks = incClock v uid
           mapM_ (\(Host h _ _) ->  forkIO $ sendMsg h p (newMsg newtoadd newtodel [] [] newclocks)) $ currentHosts c
           putMVar d (State s c newclocks (NetState (Host h uid (HostClock hu (hc+1))) p ld))

sendOperations :: State -> [HostClock] -> String -> IO ()
sendOperations s [] h = return ()
sendOperations (State (Set sa sr) (Cluster ca cr) hcs (NetState hst p ld)) (v:vs) h = 
        do sendMsg h p $ newMsg (filter (\(Tag _ _ _ clk) -> clk == v) sa)
                                (filter (\(Tag _ _ _ clk) -> clk == v) sr)
                                (filter (\(Host _ _ clk)  -> clk == v) ca)
                                (filter (\(Host _ _ clk)  -> clk == v) cr)
                                (vclkstep hcs (vs))
           sendOperations (mergeVClock [v] (State (Set sa sr) (Cluster ca cr) hcs (NetState hst p ld))) vs h
        where vclkstep :: [HostClock] -> [HostClock] -> [HostClock]
              vclkstep mv lv = filter (\(HostClock _ c) -> c >= 0) $ foldl (\acc (HostClock u c) -> decClock acc u) mv lv

sendMsg :: String -> NS.ServiceName -> Msg -> IO ()
sendMsg h p m = do
        addrInfo <- NS.getAddrInfo Nothing (Just h) (Just p)
        let serverAddr = head addrInfo
        sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
        NS.connect sock (NS.addrAddress serverAddr)
        NS.send sock (msgToBlob m)
        sClose sock

addSelf :: MVar State -> String -> NS.ServiceName -> IO ()
addSelf d host port = do
        (State s (Cluster a r) v (NetState (Host h uid hv) p _)) <- readMVar d
        forkIO $ sendMsg host port (newMsg [] [] [(Host h uid hv)] [] [hv])
        return ()
