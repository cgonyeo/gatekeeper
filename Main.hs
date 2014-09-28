{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Hex as H
import qualified Network.Socket as NS
import qualified Data.ProtocolBuffers as P
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as SP
import qualified Data.UUID as U
import qualified Data.UUID.V1 as U1
import Network
import Control.Concurrent
import GHC.TypeLits
import System.Environment
import System.IO
import System.Exit

import Gatekeeper.Network
import Gatekeeper.CRDT
import Gatekeeper.LDAP

getInitData :: IO (MVar State)
getInitData = do
       m <- newEmptyMVar
       putMVar m (State (Set [Tag "buttlord" "33333" "69696969"] []) (Cluster [] []) (NetState "" "0" NotAMember))
       return m

start progName args d = do modifyMVar_ d (\(State s c (NetState _ _ m)) -> return $ State s c (NetState host port m))
                           s <- listenOn $ PortNumber $ fromIntegral $ (read port :: Int)
                           netloop s d
                           return ()
                           where port = (args !! 1)
                                 host = (args !! 0)

main = do
        progName <- getProgName
        args <- getArgs
        d <- getInitData
        case (length args) of
            2 -> do (Just uid) <- U1.nextUUID
                    modifyMVar_ d (\(State s (Cluster a r) (NetState h p _)) -> return (State s (Cluster ((Host (args !! 0) (show uid)):a) r) (NetState h p Member)))
                    forkIO $ start progName args d
            3 -> do forkIO $ do askForNodes (args !! 2) (args !! 1)
                                modifyMVar_ d (\(State s c (NetState h p _)) -> return (State s c (NetState h p Joining)))
                    forkIO $ start progName args d
            _ -> do putStrLn $ "Usage: " ++ progName ++ " <hostname> <portnum> (<known host>)"
                    exitFailure
        ldaploop d
