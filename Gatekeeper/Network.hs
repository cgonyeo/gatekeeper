module Gatekeeper.Network ( askForNodes
                          , sendAddDeltas
                          , sendDelDeltas
                          , netloop
                          ) where

import qualified Data.ProtocolBuffers as P

import Control.Concurrent
import Network
import System.IO

import Gatekeeper.CRDT
import Gatekeeper.Protobufs
import Gatekeeper.NetUtils

netloop :: Socket -> MVar State -> IO b
netloop s d = do
        (handle,c,_) <- accept s
        forkIO $ client handle d =<< lookupHost c
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
        case () of
            _ | oper <= 3 -> modifyMVar_ d (\s -> return $ (clockTick s n))
        modifyMVar_ d (\(State s c (NetState h p v m)) -> case () of
                          _ | oper == 2 && m == Joining   
                                -> return $ (State s c (NetState h p v Joining))
                            | oper == 3 && m == Receiving 
                                -> return $ (State s c (NetState h p v Receiving))
                            | otherwise
                                -> return $ (State s c (NetState h p v m)))
        (State s c (NetState (Host h u) p v m)) <- readMVar d
        putStrLn $ "Operation completed for " ++ n ++ ". Current state:"
        putStrLn $ (show s)
        putStrLn $ (show c)
        putStrLn $ (show (NetState (Host h u) p v m))
        if (m == Member) && (not $ h `inCluster` c)
            then do 
                    forkIO $ addSelf d
                    return ()
            else return ()
        where (Just tags) = msgToTags msg
              (Just hosts) = msgToHosts msg
              oper = P.getField $ msgoper msg
