module Gatekeeper.Network ( askForNodes
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
        putStrLn $ "\nRequest received from " ++ n
        case (mta,mtd,mha,mhd,mvc) of
            (Just ta,Just td, Just ha, Just hd, Just vc) ->
                do (State s c (NetState (Host h u) p v m)) <- takeMVar d
                   putStrLn $ (show $ length ta) ++ " tags to add\n"
                           ++ (show $ length td) ++ " tags to del\n"
                           ++ (show $ length ha) ++ " hosts to add\n"
                           ++ (show $ length hd) ++ " hosts to del\n"
                           ++ (show $ length vc) ++ " hosts in the reported vector clock\n"
                   let statemod = addManyTags ta
                                . removeManyTags td
                                . addManyHosts ha
                                . removeManyHosts hd
                   let (sWClks,rx1) = mergeVClock (State s c (NetState (Host h u) p v m)) vc
                   let rx2 = rxreq2 v vc
                   if rx1 || rx2
                       then forkIO $ sendState d n
                       else  myThreadId
                   if m /= Member
                       then putMVar d $ statemod $ changeMembership Member sWClks
                       else putMVar d $ statemod sWClks
            _ -> putStrLn $ "Invalid message: " ++ (show (mta,mtd,mha,mhd,mvc))
        (State s c (NetState (Host h u) p v m)) <- readMVar d
        putStrLn $ "Operation completed for " ++ n 
                      ++ ". Current state:\n" ++ (show s) 
                                      ++ "\n" ++ (show c) 
                                      ++ "\n" ++ (show (NetState (Host h u) p v m))
        if (m == Member) && (not $ u `inCluster` c)
            then do putStrLn "Adding self..."
                    forkIO $ addSelf d
            else myThreadId
        return ()
        where mta = msgToAddTags  msg
              mtd = msgToDelTags  msg
              mha = msgToAddHosts msg
              mhd = msgToDelHosts msg
              mvc = msgToVClocks  msg
