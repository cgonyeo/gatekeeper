module Gatekeeper.CRDT ( Tag (Tag)
                       , Set (Set)
                       , Host (Host)
                       , Cluster (Cluster)
                       , HostClock (HostClock)
                       , Membership (NotAMember,Joining,Receiving,Member)
                       , NetState (NetState)
                       , State (State)
                       , incClock
                       , clockTick
                       , isInSet
                       , inCluster
                       , addTag
                       , addManyTags
                       , removeTag
                       , removeManyTags
                       , addHost
                       , addManyHosts
                       , removeHost
                       , removeManyHosts
                       , changeMembership
                       ) where

import qualified Data.List as L

data Tag = Tag { username :: String
               , tagId    :: String
               , tagUid   :: String   --tagUid should be unique, allowing any number of addition/removals of the same username/tadId
               } deriving (Show,Eq)

data Set = Set [Tag] [Tag] deriving (Show,Eq) --Add list, remove list

data Host = Host { nhostname :: String 
                 , nhostUid  :: String
                 } deriving (Show,Eq)
data Cluster = Cluster [Host] [Host] deriving (Show,Eq)

data HostClock = HostClock { hUid :: String
                           , vclock   :: Int
                           } deriving (Show,Eq)

data Membership = NotAMember | Joining | Receiving | Member deriving (Show,Eq)

data NetState = NetState { myHost     :: Host
                         , port       :: String
                         , clock      :: [HostClock]
                         , membership :: Membership
                         } deriving (Show,Eq)

data State = State Set Cluster NetState deriving (Show,Eq)

--Given a HostClock list, and a host's uid, increment that host's clock
incClock :: [HostClock] -> String -> [HostClock]
incClock ((HostClock u c):xs) uid = 
        if u == uid
            then ((HostClock u (c + 1)):xs)
            else ((HostClock u c):(incClock xs uid))

--Given a State and a host's ip, increment that host's clock
clockTick :: State -> String -> State
clockTick (State s (Cluster a r) (NetState h p vclock m)) host =
        (State s (Cluster a r) (NetState h p (incClock vclock uid) m))
        where uid = uidForHost (filter (\e -> not $ e `elem` r) a) host

--Given a host's ip, look up it's uid
uidForHost :: [Host] -> String -> String
uidForHost [] host = ""
uidForHost ((Host h u):xs) host = if h == host
                                      then h
                                      else uidForHost xs host

isInSet :: State -> String -> String -> Bool
isInSet (State (Set a r) _ _) u t = 
       (length tags2) > 0
       where tags = filter (\(Tag user id _) -> (user == u) && (id == t)) a
             tags2 = filter (\tag -> not (tag `elem` r)) tags

inCluster :: String -> Cluster -> Bool
inCluster h (Cluster a r) = h `anyElem` a && not (h `anyElem` r)
        where anyElem s l = foldl (\acc (Host h _) -> (h == s) || acc) False l

addTag :: State -> Tag -> State
addTag (State (Set a r) c n) e = if e `elem` a
                                     then State (Set a r) c n
                                     else State (Set (e:a) r) c n

addManyTags :: State -> [Tag] -> State
addManyTags state tags = foldl (\s t -> addTag s t) state tags

removeTag :: State -> Tag -> State
removeTag (State (Set a r) c n) e = if e `elem` r
                                        then State (Set a r) c n
                                        else State (Set a (e:r)) c n

removeManyTags :: State -> [Tag] -> State
removeManyTags state tags = foldl (\s t -> removeTag s t) state tags

addHost :: State -> Host -> State
addHost (State s (Cluster a r) n) e = if e `elem` a
                                          then State s (Cluster a r) n
                                          else State s (Cluster (e:a) r) n

addManyHosts :: State -> [Host] -> State
addManyHosts state hosts = foldl (\s h -> addHost s h) state hosts

removeHost :: State -> Host -> State
removeHost (State s (Cluster a r) n) e = if e `elem` r
                                             then State s (Cluster a r) n
                                             else State s (Cluster a (e:r)) n

removeManyHosts :: State -> [Host] -> State
removeManyHosts state hosts = foldl (\s h -> removeHost s h) state hosts

changeMembership :: State -> Membership -> State
changeMembership (State s c (NetState h p v _)) m = (State s c (NetState h p v m))
