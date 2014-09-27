{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.List as L
import qualified Data.Int as I
import qualified Data.ProtocolBuffers as P
import qualified Data.Text as T
import qualified Data.Hex as H
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as SP
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import GHC.Generics (Generic)
import GHC.TypeLits
import Network
import qualified Network.Socket as NS
import System.Environment
import System.IO

data Tag = Tag { username :: String
               , tagId    :: String
               , tagUid   :: String   --tagUid should be unique, allowing any number of addition/removals of the same username/tadId
               } deriving (Show,Eq)

data Set = Set [Tag] [Tag] deriving (Show,Eq) --Add list, remove list

data Host = Host { nhostname :: String 
                 , nhostUid  :: String
                 } deriving (Show,Eq)
data Cluster = Cluster [Host] [Host] deriving (Show,Eq)

data State = State Set Cluster deriving (Show,Eq)

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
                          uids  = map T.unpack $ P.getField $ msgtuids  msg

msgToHosts :: Msg -> Maybe [Host]
msgToHosts msg = if (length hosts) == (length uids)
                    then Just $ zipWith (\host uid -> (Host host uid)) hosts uids
                    else Nothing
                    where hosts = map T.unpack $ P.getField $ msghosts msg
                          uids  = map T.unpack $ P.getField $ msghuids msg

getInitData :: IO (MVar State)
getInitData = do
       m <- newEmptyMVar
       putMVar m (State (Set [] []) (Cluster [] []))
       return m

lookup :: Set -> Tag -> Bool
lookup (Set a r) e = e `elem` a && not (e `elem` r)

addTag :: State -> Tag -> State
addTag (State (Set a r) cluster) e = if e `elem` a
                                      then State (Set a r) cluster
                                      else State (Set (e:a) r) cluster

addManyTags :: State -> [Tag] -> State
addManyTags state tags = foldl (\s t -> addTag s t) state tags

removeTag :: State -> Tag -> State
removeTag (State (Set a r) cluster) e = if e `elem` r
                                         then State (Set a r) cluster
                                         else State (Set a (e:r)) cluster

removeManyTags :: State -> [Tag] -> State
removeManyTags state tags = foldl (\s t -> removeTag s t) state tags

addHost :: State -> Host -> State
addHost (State set (Cluster a r)) e = if e `elem` a
                                      then State set (Cluster a r)
                                      else State set (Cluster (e:a) r)

addManyHosts :: State -> [Host] -> State
addManyHosts state hosts = foldl (\s h -> addHost s h) state hosts

removeHost :: State -> Host -> State
removeHost (State set (Cluster a r)) e = if e `elem` r
                                         then State set (Cluster a r)
                                         else State set (Cluster a (e:r))

removeManyHosts :: State -> [Host] -> State
removeManyHosts state hosts = foldl (\s h -> removeHost s h) state hosts

subset :: (Eq a) => [a] -> [a] -> Bool
subset l1 l2 = foldl (\acc e -> (e `elem` l2) || acc) False l1

compare :: Set -> Set -> Bool
compare (Set sa sr) (Set ta tr) = sa `subset` ta || sr `subset` tr

merge :: Set -> Set -> Set
merge (Set sa sr) (Set ta tr) = Set ua ur
                    where ua = L.union sa ta
                          ur = L.union sr tr

sendNodes d l p = do 
                 (State _ (Cluster a r)) <- readMVar d
                 let msg1 = Msg { msgoper  = P.putField 2
                                , msgtags  = P.putField []
                                , msgusers = P.putField []
                                , msgtuids = P.putField []
                                , msghosts = P.putField $ map (\(Host loc _) -> T.pack loc) a
                                , msghuids = P.putField $ map (\(Host loc _) -> T.pack loc) a
                                }
                 sendMsg l p msg1
                 let msg2 = Msg { msgoper  = P.putField 3
                                , msgtags  = P.putField []
                                , msgusers = P.putField []
                                , msgtuids = P.putField []
                                , msghosts = P.putField $ map (\(Host loc _) -> T.pack loc) r
                                , msghuids = P.putField $ map (\(Host loc _) -> T.pack loc) r
                                }
                 sendMsg l p msg2

handleMsg msg d l p = do
        case oper of
            0 -> modifyMVar_ d (\s -> return $ addManyTags s tags)      -- 0: add these tags
            1 -> modifyMVar_ d (\s -> return $ removeManyTags s tags)   -- 1: remove these tags
            2 -> modifyMVar_ d (\s -> return $ addManyHosts s hosts)    -- 2: add these hosts
            3 -> modifyMVar_ d (\s -> return $ removeManyHosts s hosts) -- 3: remove these hosts
            4 -> do forkIO $ sendNodes d l p; return ()                  -- 4: They requested we share all our hostnames with them
            5 -> return ()                                              -- 5: Just receiving some hostnames
        s <- readMVar d
        putStrLn "Update received. New data:"
        putStrLn $ show s
        where (Just tags) = msgToTags msg
              (Just hosts) = msgToHosts msg
              oper = P.getField $ msgoper msg

client h d l p = do
        m <- hGetContents h
        let rslt = (G.runGet P.decodeMessage =<< H.unhex (C.pack m) :: Either String Msg)
        case rslt of
            Left s -> putStrLn s
            Right msg -> handleMsg msg d l p
        hClose h

netloop s d p = do
        (h,l,_) <- accept s
        putStrLn l
        forkIO $ client h d l p
        netloop s d p

sendMsg h p m = do
        addrInfo <- NS.getAddrInfo Nothing (Just h) (Just p)
        let serverAddr = head addrInfo
        sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
        NS.connect sock (NS.addrAddress serverAddr)
        NS.send sock $ C.unpack $ fmap H.hex SP.runPut $ P.encodeMessage m
        sClose sock

main = do
        progName <- getProgName
        args <- getArgs
        if length args /= 1
            then putStrLn $ "Usage: " ++ progName ++ " <portnum>"
            else do d <- getInitData
                    s <- listenOn $ PortNumber $ fromIntegral $ read (args !! 0)
                    netloop s d $ read (args !! 0)
                    return ()
