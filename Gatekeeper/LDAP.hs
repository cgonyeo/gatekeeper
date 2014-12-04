module Gatekeeper.LDAP ( fetchTagChanges
                       , ldaploop
                       ) where

import Control.Concurrent
import LDAP.Init
import LDAP.Search
import LDAP.Constants
import System.Random
import Network.Socket

import qualified Data.UUID.V1 as U1

import Gatekeeper.CRDT
import Gatekeeper.NetUtils

minute :: Double
minute = 1000000.0 * 60.0

ldaploop :: MVar State -> Double -> Double -> IO ()
ldaploop d ldu ldl = do
        sleepamt <- randomRIO (truncate $ minute * ldu, truncate $ minute * ldl)
        threadDelay sleepamt
        (toadd,todel) <- fetchTagChanges d
        forkIO $ batch d toadd todel
        ldaploop d ldu ldl

batch :: MVar State -> [Tag] -> [Tag] -> IO ()
batch d [] [] = return ()
batch d toadd todel = do
        let (currAddBatch,nextAddBatch) = splitAt 50 toadd
        let (currDelBatch,nextDelBatch) = splitAt 50 todel
        forkIO $ sendTagDeltas d currAddBatch currDelBatch
        batch d nextAddBatch nextDelBatch

getUser :: [(String, [String])] -> Maybe String
getUser [] = Nothing
getUser (("uid", vals):attrs)
    | not $ null vals = Just $ head vals
    | otherwise         = getUser attrs
getUser (_:attrs) = getUser attrs

getId :: [(String, [String])] -> Maybe String
getId [] = Nothing
getId (("roomNumber", vals):attrs)
    | not $ null vals = Just $ head vals
    | otherwise         = getId attrs
getId (_:attrs) = getId attrs

getAttrs :: LDAPEntry -> [(String, [String])]
getAttrs (LDAPEntry _ attrs) = attrs

genAddDeltas :: State -> [(String,String)] -> IO [Tag]
genAddDeltas s p = do
        let toadd = filter (\(user, id) -> not $ isInSet s user id) p
        uids <- mapM (const U1.nextUUID) toadd
        return $ zipWith (\(user,id) (Just uid) -> Tag user id (show uid) (HostClock "" 0)) toadd uids

genDelDeltas :: State -> [(String,String)] -> IO [Tag]
genDelDeltas (State s _ _ _) p =
        return $ filter (\(Tag user id _ _) -> (user,id) `notElem` p) $ currentTags s

fetchTagChanges :: MVar State -> IO ([Tag],[Tag])
fetchTagChanges d = do
        (State _ _ _ (NetState _ _ (LdapInfo url user pass))) <- readMVar d
        l <- ldapInitialize url
        ldapSimpleBind l user pass
        results <- ldapSearch l (Just "dc=csh,dc=rit,dc=edu") LdapScopeSubtree Nothing (LDAPAttrList ["uid","roomNumber"]) False
        let people = [(getUser a, getId a) | a <- map (\(LDAPEntry _ attrs) -> attrs) results]
        let peoplefiltered = filter (\(u,i) -> case (u,i) of
                                                   (Just _,Just _) -> True
                                                   (_,_)           -> False) people
        let peoplemapped = map (\(Just u, Just i) -> (u,i)) peoplefiltered
        s <- takeMVar d
        addDelta <- genAddDeltas s peoplemapped
        delDelta <- genDelDeltas s peoplemapped
        putMVar d s
        putStrLn $ "LDAP add: " ++ show (length addDelta)
        putStrLn $ "LDAP del: " ++ show (length delDelta)
        return (addDelta,delDelta)
