module Gatekeeper.CRDT ( Tag (Tag)
                       , Set (Set)
                       , Host (Host)
                       , Cluster (Cluster)
                       , HostClock (HostClock)
                       , NetState (NetState)
                       , LdapInfo (LdapInfo)
                       , State (State)
                       , incClock
                       , decClock
                       , currentTags
                       , currentHosts
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
                       , mergeVClock
                       , rxreq
                       ) where

import qualified Data.List as L

data HostClock = HostClock { hUid     :: String
                           , vclock   :: Int
                           } deriving (Show,Eq)

data Tag = Tag { username :: String
               , tagId    :: String
               , tagUid   :: String    --tagUid should be unique, allowing any number of addition/removals of the same username/tadId
               , tvclk    :: HostClock --The clock tick which put this tag where it is
               } deriving (Show,Eq)

data Set = Set [Tag] [Tag] deriving (Show,Eq) --Add list, remove list

data Host = Host { nhostname :: String 
                 , nhostUid  :: String
                 , hvclk     :: HostClock --The clock tick which put this host where it is
                 } deriving (Show,Eq)

data Cluster = Cluster [Host] [Host] deriving (Show,Eq)

data LdapInfo = LdapInfo { ldapurl      :: String
                         , ldapusername :: String
                         , ldappassword :: String
                         } deriving (Show,Eq)

data NetState = NetState { myHost   :: Host
                         , port     :: String
                         , ldapinfo :: LdapInfo
                         } deriving (Show,Eq)

data State = State Set Cluster [HostClock] NetState deriving (Show,Eq)

--Given a HostClock list, and a host's uid, increment that host's clock
incClock :: [HostClock] -> String -> [HostClock]
incClock ((HostClock u c):xs) uid = 
        if u == uid
            then ((HostClock u (c + 1)):xs)
            else ((HostClock u c):(incClock xs uid))

--Given a HostClock list, and a host's uid, decrement that host's clock
--Only used for playing back operations to another node
decClock :: [HostClock] -> String -> [HostClock]
decClock ((HostClock u c):xs) uid = 
        if u == uid
            then ((HostClock u (c - 1)):xs)
            else ((HostClock u c):(decClock xs uid))

isInSet :: State -> String -> String -> Bool
isInSet (State s _ _ _) u t = (length tags) > 0
       where tags = filter (\(Tag user id uid v) -> (user == u) && (id == t)) $ currentTags s

currentTags :: Set -> [Tag]
currentTags (Set a r) = filter (\(Tag _ _ u1 _) 
                                 -> foldl (\acc (Tag _ _ u2 _) 
                                            -> acc && u1 /= u2
                                          ) True r
                               ) a

currentHosts :: Cluster -> [Host]
currentHosts (Cluster a r) = filter (\(Host _ u1 _) 
                                      -> foldl (\acc (Host _ u2 _) 
                                                 -> acc && u1 /= u2
                                               ) True r
                                    ) a

inCluster :: String -> Cluster -> Bool
inCluster u (Cluster a r) = u `anyElem` a && not (u `anyElem` r)
        where anyElem s l = foldl (\acc (Host _ u _) -> (u == s) || acc) False l

addTag :: State -> Tag -> State
addTag (State (Set a r) c v n) e = if e `elem` a
                                     then State (Set a r) c v n
                                     else State (Set (e:a) r) c v n

addManyTags :: [Tag] -> State -> State
addManyTags tags state = foldl (\s t -> addTag s t) state tags

removeTag :: State -> Tag -> State
removeTag (State (Set a r) c v n) e = if e `elem` r
                                        then State (Set a r) c v n
                                        else State (Set a (e:r)) c v n

removeManyTags :: [Tag] -> State -> State
removeManyTags tags state = foldl (\s t -> removeTag s t) state tags

addHost :: State -> Host -> State
addHost (State s (Cluster a r) v n) e = if e `elem` a
                                          then State s (Cluster a r) v n
                                          else State s (Cluster (e:a) r) v n

addManyHosts :: [Host] -> State -> State
addManyHosts hosts state = foldl (\s h -> addHost s h) state hosts

removeHost :: State -> Host -> State
removeHost (State s (Cluster a r) v n) e = if e `elem` r
                                             then State s (Cluster a r) v n
                                             else State s (Cluster a (e:r)) v n

removeManyHosts :: [Host] -> State -> State
removeManyHosts hosts state = foldl (\s h -> removeHost s h) state hosts

mergeVClock :: [HostClock] -> State -> State
mergeVClock clks (State s c v n) = (State s c newclks n)
        where newclks = foldl (\vs clk -> merge vs clk) v clks
              merge :: [HostClock] -> HostClock -> [HostClock]
              merge [] v = [v]
              merge ((HostClock u1 c1):xs) (HostClock u2 c2) =
                      if u1 == u2
                          then ((HostClock u1 (max c1 c2)):xs)
                          else ((HostClock u1 c1):(merge xs (HostClock u2 c2)))

--Takes the added and removed tags and hosts, our clocks, the reported clocks,
--the reporting uid, and returns two [HostClock]s. The first one is operations
--we're missing, and the second one is operations the reporting host is missing.
rxreq :: [Tag] -> [Tag] -> [Host] -> [Host] -> [HostClock] -> [HostClock] -> ([HostClock],[HostClock])
rxreq ta td ha hd mv rv = (reverse $ ticksNotHere ta td ha hd (newTicks mv rv),reverse $ newTicks rv mv)
    where newTicks :: [HostClock] -> [HostClock] -> [HostClock]
          newTicks mv rv = foldl (\acc (HostClock ru rc) 
                                   -> case (filter (\(HostClock mu _) -> ru == mu) mv) of
                                        [(HostClock mu mc)] -> if rc > mc
                                                                   then foldl (\acc x -> (x:acc)) acc (map (\n -> (HostClock ru n)) [(mc+1)..rc])
                                                                   else acc
                                        [] -> foldl (\acc x -> (x:acc)) acc (map (\n -> (HostClock ru n)) [0..rc])
                                 ) [] rv
          ticksNotHere :: [Tag] -> [Tag] -> [Host] -> [Host] -> [HostClock] -> [HostClock]
          ticksNotHere ta td ha hd vs = filter (\v1
                                                 -> not $ (foldl (\acc (Tag _ _ _ v2) -> acc || v1 == v2) False ta)
                                                       || (foldl (\acc (Tag _ _ _ v2) -> acc || v1 == v2) False td)
                                                       || (foldl (\acc (Host _ _ v2)  -> acc || v1 == v2) False ha)
                                                       || (foldl (\acc (Host _ _ v2)  -> acc || v1 == v2) False hd)) vs
