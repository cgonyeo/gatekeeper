module GateCRDT ( Tag (Tag)
                , Set (Set)
                , Host (Host)
                , Cluster (Cluster)
                , Membership (NotAMember,Joining,Receiving,Member)
                , NetState (NetState)
                , State (State)
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

data Membership = NotAMember | Joining | Receiving | Member deriving (Show,Eq)

data NetState = NetState { hostname :: String
                         , port :: String
                         , membership :: Membership
                         } deriving (Show,Eq)

data State = State Set Cluster NetState deriving (Show,Eq)

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
