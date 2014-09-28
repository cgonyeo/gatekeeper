module GateNetwork ( askForNodes
                   , sendAddDeltas
                   , sendDelDeltas
                   , netloop
                   ) where

import qualified Data.UUID as U
import qualified Data.UUID.V1 as U1
import qualified Data.ProtocolBuffers as P

import Control.Concurrent
import Network
import qualified Network.Socket as NS
import System.IO

import GateCRDT
import GateProtobufs

netloop :: Socket -> MVar State -> IO b
netloop s d = do
        (handle,c,_) <- accept s
        forkIO $ client handle d c
        netloop s d

client :: Handle -> MVar State -> String -> IO ()
client handle d c = do
        m <- hGetContents handle
        case (blobToMsg m) of
            Left s -> putStrLn $ "Couldn't parse message from client: " ++ s
            Right msg -> handleMsg msg d c
        hClose handle

handleMsg :: Msg -> MVar State -> String -> IO ()
handleMsg msg d n = do
        putStrLn $ "\nRequest received from " ++ n ++ " for operation " ++ (show oper)
        case oper of
         -- 0: add these tags
            0 -> modifyMVar_ d (\s -> return $ addManyTags s tags)
         -- 1: remove these tags
            1 -> modifyMVar_ d (\s -> return $ removeManyTags s tags)
         -- 2: add these hosts
            2 -> modifyMVar_ d (\s -> return $ addManyHosts s hosts)
         -- 3: remove these hosts
            3 -> modifyMVar_ d (\s -> return $ removeManyHosts s hosts)
         -- 4: They requested we share all our known nodes with them
            4 -> do forkIO $ sendNodes d n; return ()
        modifyMVar_ d (\(State s c (NetState h p m)) -> case () of
                          _ | oper == 2 && m == Joining   -> return $ (State s c (NetState h p Joining))
                            | oper == 3 && m == Receiving -> return $ (State s c (NetState h p Receiving))
                            | otherwise                   -> return $ (State s c (NetState h p m)))
        (State s c (NetState h p m)) <- readMVar d
        putStrLn $ "Operation completed for " ++ n ++ ". Current state:"
        putStrLn $ (show s)
        putStrLn $ (show c)
        putStrLn $ (show (NetState h p m))
        if (m == Member) && (not $ h `inCluster` c)
            then do 
                    forkIO $ addSelf d
                    return ()
            else return ()
        where (Just tags) = msgToTags msg
              (Just hosts) = msgToHosts msg
              oper = P.getField $ msgoper msg

sendNodes :: MVar State -> String -> IO ()
sendNodes d c = do 
                (State _ (Cluster a r) (NetState _ p _)) <- readMVar d
                sendMsg c p (addHostsMsg a)
                sendMsg c p (delHostsMsg r)

addSelf :: MVar State -> IO ()
addSelf d = do
        (Just uid) <- U1.nextUUID
        (State _ (Cluster a r) (NetState h p _)) <- readMVar d
        let lst = filter (\h -> not (h `elem` r)) a
        modifyMVar_ d (\(State s (Cluster a r) n) -> return (State s (Cluster ((Host h (show uid)):a) r) n))
        mapM_ (\(Host hst _) -> forkIO $ addHostToTarget (Host h (show uid)) hst p) lst

addHostToTarget :: Host -> String -> NS.ServiceName -> IO ()
addHostToTarget h t p = sendMsg t p (addHostsMsg [h])

askForNodes :: String -> NS.ServiceName -> IO ()
askForNodes l p = do
        sendMsg l p reqNodesMsg

sendAddDeltas :: MVar State -> [Tag] -> IO ()
sendAddDeltas d toadd = do 
                 (State s (Cluster a r) (NetState _ p _)) <- readMVar d
                 let hosts = filter (\h -> not (h `elem` r)) a
                 mapM_ (\(Host h _) -> forkIO $ sendMsg h p (addTagsMsg toadd)) hosts

sendDelDeltas :: MVar State -> [Tag] -> IO ()
sendDelDeltas d todel = do 
                 (State s (Cluster a r) (NetState _ p _)) <- readMVar d
                 let hosts = filter (\h -> not (h `elem` r)) a
                 mapM_ (\(Host h _) -> forkIO $ sendMsg h p (delTagsMsg todel)) hosts

sendMsg :: String -> NS.ServiceName -> Msg -> IO ()
sendMsg h p m = do
        addrInfo <- NS.getAddrInfo Nothing (Just h) (Just p)
        let serverAddr = head addrInfo
        sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
        NS.connect sock (NS.addrAddress serverAddr)
        NS.send sock (msgToBlob m)
        sClose sock
