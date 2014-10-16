module Gatekeeper.CRDT ( Tag (Tag)
                       , Set (Set)
                       , Host (Host)
                       , Cluster (Cluster)
                       , HostClock (HostClock)
                       , Membership (NotAMember,Joining,Receiving,Member)
                       , NetState (NetState)
                       , State (State)
                       , incClock
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
                       , mergeVClock
                       , rxreq2
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

data HostClock = HostClock { hUid     :: String
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

isInSet :: State -> String -> String -> Bool
isInSet (State (Set a r) _ _) u t = 
       (length tags2) > 0
       where tags = filter (\(Tag user id _) -> (user == u) && (id == t)) a
             tags2 = filter (\tag -> not (tag `elem` r)) tags

inCluster :: String -> Cluster -> Bool
inCluster u (Cluster a r) = u `anyElem` a && not (u `anyElem` r)
        where anyElem s l = foldl (\acc (Host _ u) -> (u == s) || acc) False l

addTag :: State -> Tag -> State
addTag (State (Set a r) c n) e = if e `elem` a
                                     then State (Set a r) c n
                                     else State (Set (e:a) r) c n

addManyTags :: [Tag] -> State -> State
addManyTags tags state = foldl (\s t -> addTag s t) state tags

removeTag :: State -> Tag -> State
removeTag (State (Set a r) c n) e = if e `elem` r
                                        then State (Set a r) c n
                                        else State (Set a (e:r)) c n

removeManyTags :: [Tag] -> State -> State
removeManyTags tags state = foldl (\s t -> removeTag s t) state tags

addHost :: State -> Host -> State
addHost (State s (Cluster a r) n) e = if e `elem` a
                                          then State s (Cluster a r) n
                                          else State s (Cluster (e:a) r) n

addManyHosts :: [Host] -> State -> State
addManyHosts hosts state = foldl (\s h -> addHost s h) state hosts

removeHost :: State -> Host -> State
removeHost (State s (Cluster a r) n) e = if e `elem` r
                                             then State s (Cluster a r) n
                                             else State s (Cluster a (e:r)) n

removeManyHosts :: [Host] -> State -> State
removeManyHosts hosts state = foldl (\s h -> removeHost s h) state hosts

changeMembership :: Membership -> State -> State
changeMembership m (State s c (NetState h p v _)) = (State s c (NetState h p v m))

mergeVClock :: State -> [HostClock] -> (State,Bool)
mergeVClock (State s c (NetState h p v m)) clks = ((State s c (NetState h p newclks m)), r)
        where (newclks,r) = foldl (\(vs,rx) clk -> (merge vs clk,rx || (rxreq1 vs clk))) (v,False) clks
              merge :: [HostClock] -> HostClock -> [HostClock]
              merge [] v = [v]
              merge ((HostClock u1 c1):xs) (HostClock u2 c2) =
                      if u1 == u2
                          then ((HostClock u1 (max c1 c2)):xs)
                          else ((HostClock u1 c1):(merge xs (HostClock u2 c2)))
              rxreq1 :: [HostClock] -> HostClock -> Bool
              rxreq1 [] v = False
              rxreq1 ((HostClock u1 c1):xs) (HostClock u2 c2) =
                    if u1 == u2 && c1 > c2 
                        then True
                        else rxreq1 xs (HostClock u2 c2)

--there are 2 cases in which we should transmit information to reconcile
--divergent states between nodes. rxreq1, above, handles 1 case and rxreq2,
--below, handles the other. Case 1 is where any node in the reported state has a
--lower clock than in our internal state. Case 2 is where any node in our
--internal state does not exist in the reported state. Either case means that
--we have data 
rxreq2 :: [HostClock] -> [HostClock] -> Bool
rxreq2 [] _ = False
rxreq2 ((HostClock u1 _):xs) vs = if foldl (\exists (HostClock u2 _) -> exists && (u1 /= u2)) True vs
                                      then True
                                      else rxreq2 xs vs
