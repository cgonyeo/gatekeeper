module Gatekeeper.Network ( netloop
                          ) where

import qualified Data.ProtocolBuffers as P

import Control.Concurrent
import Network
import System.IO

import Gatekeeper.CRDT
import Gatekeeper.Protobufs
import Gatekeeper.NetUtils

netloop :: MVar State -> Socket -> IO b
netloop d s = do
        (handle,c,_) <- accept s
        forkIO $ client handle d =<< lookupHost c
        netloop d s

client :: Handle -> MVar State -> String -> IO ()
client handle d c = do
        m <- hGetContents handle
        case (blobToMsg m) of
            Left s -> putStrLn $ "Couldn't parse message from client: " ++ s
            Right msg -> handleMsg msg d c
        hClose handle

handleMsg :: Msg -> MVar State -> String -> IO ()
handleMsg msg d t = do
        putStrLn $ "\nRequest received from " ++ t
        if ta == [] && td == [] && ha == [] && hd == []
            then do putStrLn "Request is a heartbeat"
                    (State s c v (NetState h p ld hbu hbl)) <- readMVar d
                    let (welack,theylack) = rxreq ta td ha hd v vc
                    case () of
                        _ | welack == [] && theylack == [] -> do myThreadId
                                                                 putStrLn "We're in sync"
                          | welack == [] && theylack /= [] -> do forkIO $ sendOperations (State s c v (NetState h p ld hbu hbl)) theylack t
                                                                 putStrLn $ "They lack " ++ (show theylack)
                          | welack /= [] && theylack == [] -> do forkIO $ sendMsg t p $ newMsg [] [] [] [] v
                                                                 putStrLn $ "We lack " ++ (show welack)
                          | welack /= [] && theylack /= [] -> do forkIO $ do sendOperations (State s c v (NetState h p ld hbu hbl)) theylack t
                                                                             sendMsg t p $ newMsg [] [] [] [] v
                                                                 putStrLn $ "We lack " ++ (show welack)
                                                                 putStrLn $ "They lack " ++ (show theylack)
                    return ()
            else do putStrLn $ (show $ length ta) ++ " tags to add\n"
                            ++ (show $ length td) ++ " tags to del\n"
                            ++ (show $ length ha) ++ " hosts to add\n"
                            ++ (show $ length hd) ++ " hosts to del\n"
                            ++ (show $ length vc) ++ " hosts in the reported vector clock\n"
                    let statemod = (mergeVClock vc . addManyTags ta . removeManyTags td
                                  . addManyHosts ha . removeManyHosts hd)
                    (State s c v (NetState h p ld hbu hbl)) <-  takeMVar d
                    let (welack,theylack) = rxreq ta td ha hd v vc
                    case () of
                        _ | welack == [] && theylack == [] -> putMVar d $ statemod (State s c v (NetState h p ld hbu hbl))
                          | welack == [] && theylack /= [] -> do forkIO $ sendOperations (statemod (State s c v (NetState h p ld hbu hbl))) theylack t
                                                                 putMVar d $ statemod (State s c v (NetState h p ld hbu hbl))
                                                                 putStrLn $ "They lack " ++ (show theylack)
                          | welack /= [] && theylack == [] -> do forkIO $ sendMsg t p $ newMsg [] [] [] [] v
                                                                 putMVar d (State s c v (NetState h p ld hbu hbl))
                                                                 putStrLn $ "We lack " ++ (show welack)
                          | welack /= [] && theylack /= [] -> do forkIO $ do sendOperations (statemod (State s c v (NetState h p ld hbu hbl))) theylack t
                                                                             sendMsg t p $ newMsg [] [] [] [] v
                                                                 putMVar d $ statemod (State s c v (NetState h p ld hbu hbl))
                                                                 putStrLn $ "We lack " ++ (show welack)
                                                                 putStrLn $ "They lack " ++ (show theylack)
        (State s c v (NetState h p _ _ _)) <- readMVar d
        putStrLn $ "Operation completed for " ++ t 
                   ++ ".\nNum current tags: " ++ (show $ length $ currentTags s) 
                   ++ "\nNum current hosts: " ++ (show $ length $ currentHosts c) 
                                      ++ "\n" ++ (show v)
        where ta = msgToAddTags  msg
              td = msgToDelTags  msg
              ha = msgToAddHosts msg
              hd = msgToDelHosts msg
              vc = msgToVClocks  msg
