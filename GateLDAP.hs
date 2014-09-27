module GateLDAP ( fetchTagChanges
                , ldaploop
                ) where

import Control.Concurrent
import LDAP.Init
import LDAP.Search
import LDAP.Constants
import System.Random

import qualified Data.UUID.V1 as U1

import GateCRDT

ldaploop d = do sleepamt <- randomRIO (1000000 * 10, 1000000 * 30)
                threadDelay sleepamt
                (toadd,todel) <- fetchTagChanges d
                modifyMVar_ d (\s -> return $ addManyTags s toadd)
                modifyMVar_ d (\s -> return $ removeManyTags s todel)
                ldaploop d

getUser :: [(String, [String])] -> Maybe String
getUser [] = Nothing
getUser (("uid", vals):attrs)
    | (length vals) > 0 = Just $ vals !! 0
    | otherwise = getUser attrs
getUser (_:attrs) = getUser attrs

getId :: [(String, [String])] -> Maybe String
getId [] = Nothing
getId (("uidNumber", vals):attrs)
    | (length vals) > 0 = Just $ vals !! 0
    | otherwise = getId attrs
getId (_:attrs) = getId attrs

getAttrs :: LDAPEntry -> [(String, [String])]
getAttrs (LDAPEntry _ attrs) = attrs

genAddDeltas :: State -> [(String,String)] -> IO [Tag]
genAddDeltas s p = do
        let toadd = filter (\(user, id) -> not $ isInSet s user id) p
        uids <- mapM (\_ -> U1.nextUUID) toadd
        return $ zipWith (\(user,id) (Just uid) -> Tag user id (show uid)) toadd uids

genDelDeltas :: State -> [(String,String)] -> IO [Tag]
genDelDeltas s p = do
        return $ filter (\(Tag user id _) -> not $ (user,id) `elem` p) (getActiveTags s)

fetchTagChanges :: MVar (State) -> IO ([Tag],[Tag])
fetchTagChanges d = do
        l <- ldapInitialize "ldaps://ldap.csh.rit.edu"
        ldapSimpleBind l "uid=dgonyeo,ou=users,dc=csh,dc=rit,dc=edu" "lolpassword"
        results <- ldapSearch l Nothing LdapScopeSubtree Nothing (LDAPAttrList ["uid","uidNumber"]) False
        let people = [((getUser a),(getId a)) | a <- (map (\(LDAPEntry _ attrs) -> attrs) results)]
        let peoplefiltered = filter (\(u,i) -> case (u,i) of
                                                   (Just _,Just _) -> True
                                                   (_,_) -> False) people
        let peoplemapped = map (\(Just u, Just i) -> (u,i)) peoplefiltered
        s <- takeMVar d
        addDelta <- genAddDeltas s peoplemapped
        delDelta <- genDelDeltas s peoplemapped
        putMVar d s
        putStrLn $ "LDAP add: " ++ (show addDelta)
        putStrLn $ "LDAP del: " ++ (show delDelta)
        return (addDelta,delDelta)
