module GateCRDT ( Tag (Tag)
                , Set (Set)
                , Host (Host)
                , Cluster (Cluster)
                , Membership (NotAMember,Joining,Receiving,Member)
                , State (State)
                , isInSet
                , inCluster
                , getActiveTags
                , addTag
                , addManyTags
                , removeTag
                , removeManyTags
                , addHost
                , addManyHosts
                , removeHost
                , removeManyHosts
                , changeMembership
                , memberStatus
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

data State = State Set Cluster Membership deriving (Show,Eq)

lookup :: Set -> Tag -> Bool
lookup (Set a r) e = e `elem` a && not (e `elem` r)

isInSet :: State -> String -> String -> Bool
isInSet (State (Set a r) _ _) u t = 
       (length tags2) > 0
       where tags = filter (\(Tag user id _) -> (user == u) && (id == t)) a
             tags2 = filter (\tag -> not (tag `elem` r)) tags

getActiveTags :: State -> [Tag]
getActiveTags (State (Set a r) _ _) = filter (\e -> not $ e `elem` r) a

anyElem :: String -> [Host] -> Bool
anyElem s l = foldl (\acc (Host h _) -> (h == s) || acc) False l

inCluster :: String -> State -> Bool
inCluster h (State _ (Cluster a r) _) = h `anyElem` a && not (h `anyElem` r)

addTag :: State -> Tag -> State
addTag (State (Set a r) c m) e = if e `elem` a
                                     then State (Set a r) c m
                                     else State (Set (e:a) r) c m

addManyTags :: State -> [Tag] -> State
addManyTags state tags = foldl (\s t -> addTag s t) state tags

removeTag :: State -> Tag -> State
removeTag (State (Set a r) c m) e = if e `elem` r
                                        then State (Set a r) c m
                                        else State (Set a (e:r)) c m

removeManyTags :: State -> [Tag] -> State
removeManyTags state tags = foldl (\s t -> removeTag s t) state tags

addHost :: State -> Host -> State
addHost (State s (Cluster a r) m) e = if e `elem` a
                                          then State s (Cluster a r) m
                                          else State s (Cluster (e:a) r) m

addManyHosts :: State -> [Host] -> State
addManyHosts state hosts = foldl (\s h -> addHost s h) state hosts

removeHost :: State -> Host -> State
removeHost (State s (Cluster a r) m) e = if e `elem` r
                                             then State s (Cluster a r) m
                                             else State s (Cluster a (e:r)) m

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

changeMembership :: State -> Membership -> State
changeMembership (State s c _) m = State s c m

memberStatus :: State -> Membership -> Bool
memberStatus (State _ _ m1) m2 = m1 == m2
