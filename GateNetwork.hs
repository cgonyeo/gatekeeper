{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module GateNetwork ( askForNodes
                   , sendAddDeltas
                   , sendDelDeltas
                   , netloop
                   ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Hex as H
import qualified Data.Int as I
import qualified Data.ProtocolBuffers as P
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as SP
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V1 as U1

import Control.Concurrent
import GHC.Generics (Generic)
import Network
import qualified Network.Socket as NS
import System.IO

import GateCRDT

data Msg = Msg { msgoper   :: P.Required 1 (P.Value I.Int64)
               , msgusers  :: P.Repeated 2 (P.Value T.Text)
               , msgtags   :: P.Repeated 3 (P.Value T.Text)
               , msgtuids  :: P.Repeated 4 (P.Value T.Text)
               , msghosts  :: P.Repeated 5 (P.Value T.Text)
               , msghuids  :: P.Repeated 6 (P.Value T.Text)
               } deriving (Generic,Show)

instance P.Encode Msg
instance P.Decode Msg

msgToTags :: Msg -> Maybe [Tag]
msgToTags msg = if ((length users) == (length tags)) && ((length tags) == (length uids))
                    then Just $ zipWith3 (\user tagid uid -> (Tag user tagid uid)) users tags uids
                    else Nothing
                    where users = map T.unpack $ P.getField $ msgusers msg
                          tags  = map T.unpack $ P.getField $ msgtags  msg
                          uids  = map T.unpack $ P.getField $ msgtuids msg

msgToHosts :: Msg -> Maybe [Host]
msgToHosts msg = if (length hosts) == (length uids)
                    then Just $ zipWith (\host uid -> (Host host uid)) hosts uids
                    else Nothing
                    where hosts = map T.unpack $ P.getField $ msghosts msg
                          uids  = map T.unpack $ P.getField $ msghuids msg

netloop s d h p = do
        (handle,l,_) <- accept s
        forkIO $ client handle d h p l
        netloop s d h p

client handle d h p l = do
        m <- hGetContents handle
        let rslt = (G.runGet P.decodeMessage =<< H.unhex (C.pack m) :: Either String Msg)
        case rslt of
            Left s -> putStrLn s
            Right msg -> handleMsg msg d h p l
        hClose handle

handleMsg msg d h p l = do
        putStrLn $ "\nRequest received from " ++ l ++ " for operation " ++ (show oper)
        putStrLn $ (show $ length $ P.getField $ msgusers msg)
        putStrLn $ (show $ length $ P.getField $ msgtags msg)
        putStrLn $ (show $ length $ P.getField $ msgtuids msg)
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
            4 -> do forkIO $ sendNodes d l p; return ()
        modifyMVar_ d (\s -> case oper of
                      2 -> if memberStatus s Joining
                               then return $ changeMembership s Receiving
                               else return $ s
                      3 -> if memberStatus s Receiving
                               then return $ changeMembership s Member
                               else return $ s
                      _ -> return s)
        s <- readMVar d
        putStrLn $ "Operation completed for " ++ l ++ ". Current state:"
        if (memberStatus s Member) && (not $ h `inCluster` s)
            then do 
                    forkIO $ do
                        hosts <- addSelf h p d
                        putStrLn "Added myself to: "
                        printThing hosts
                        return ()
                    return ()
            else return ()
        where (Just tags) = msgToTags msg
              (Just hosts) = msgToHosts msg
              oper = P.getField $ msgoper msg

sendNodes d l p = do 
                 (State _ (Cluster a r) _) <- readMVar d
                 putStrLn $ "Sending nodes to add: " ++ (show a)
                 let msg1 = Msg { msgoper  = P.putField 2
                                , msgtags  = P.putField []
                                , msgusers = P.putField []
                                , msgtuids = P.putField []
                                , msghosts = P.putField $ map (\(Host loc _) -> T.pack loc) a
                                , msghuids = P.putField $ map (\(Host _ uid) -> T.pack uid) a
                                }
                 sendMsg l p msg1
                 threadDelay 2000000
                 let msg2 = Msg { msgoper  = P.putField 3
                                , msgtags  = P.putField []
                                , msgusers = P.putField []
                                , msgtuids = P.putField []
                                , msghosts = P.putField $ map (\(Host loc _) -> T.pack loc) r
                                , msghuids = P.putField $ map (\(Host loc _) -> T.pack loc) r
                                }
                 sendMsg l p msg2

printThing [] = do return ()
printThing (x:xs) = do
        y <- x
        putStrLn $ show y
        printThing xs

addSelf h p d = do
        (Just uid) <- U1.nextUUID
        (State _ (Cluster a r) _) <- readMVar d
        let lst = filter (\h -> not (h `elem` r)) a
        modifyMVar_ d (\s -> return $ addHost s (Host h (show uid)))
        return $ map (\(Host hst _) -> addHostToTarget (Host h (show uid)) hst p) lst

addHostToTarget (Host h uid) t p = do
        let msg = Msg { msgoper  = P.putField 2
                      , msgtags  = P.putField []
                      , msgusers = P.putField []
                      , msgtuids = P.putField []
                      , msghosts = P.putField [T.pack h]
                      , msghuids = P.putField [T.pack uid]
                      }
        sendMsg t p msg
        return h

askForNodes l p = do
        let msg = Msg { msgoper  = P.putField 4
                      , msgtags  = P.putField []
                      , msgusers = P.putField []
                      , msgtuids = P.putField []
                      , msghosts = P.putField []
                      , msghuids = P.putField []
                      }
        sendMsg l p msg

sendAddDeltas d p toadd = do 
                 s <- readMVar d
                 let msg = Msg { msgoper  = P.putField 0
                               , msgusers = P.putField $ map (\(Tag user _ _) -> T.pack user) toadd
                               , msgtags  = P.putField $ map (\(Tag _ id _) -> T.pack id) toadd
                               , msgtuids = P.putField $ map (\(Tag _ _ uid) -> T.pack uid) toadd
                               , msghosts = P.putField []
                               , msghuids = P.putField []
                               }
                 putStrLn $ (show $ length $ P.getField $ msgusers msg)
                 putStrLn $ (show $ length $ P.getField $ msgtags msg)
                 putStrLn $ (show $ length $ P.getField $ msgtuids msg)
                 let hosts = getActiveHosts s
                 mapM_ (\(Host h _) -> sendMsg h p msg) hosts

sendDelDeltas d p todel = do 
                 s <- readMVar d
                 let msg = Msg { msgoper  = P.putField 1
                               , msgusers = P.putField $ map (\(Tag user _ _) -> T.pack user) todel
                               , msgtags  = P.putField $ map (\(Tag _ id _) -> T.pack id) todel
                               , msgtuids = P.putField $ map (\(Tag _ _ uid) -> T.pack uid) todel
                               , msghosts = P.putField []
                               , msghuids = P.putField []
                               }
                 putStrLn $ (show $ length $ P.getField $ msgusers msg)
                 putStrLn $ (show $ length $ P.getField $ msgtags msg)
                 putStrLn $ (show $ length $ P.getField $ msgtuids msg)
                 let hosts = getActiveHosts s
                 mapM_ (\(Host h _) -> sendMsg h p msg) hosts

sendMsg h p m = do
        putStrLn $ "Sending message to " ++ h
        addrInfo <- NS.getAddrInfo Nothing (Just h) (Just p)
        let serverAddr = head addrInfo
        sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
        NS.connect sock (NS.addrAddress serverAddr)
        NS.send sock $ C.unpack $ fmap H.hex SP.runPut $ P.encodeMessage m
        sClose sock
