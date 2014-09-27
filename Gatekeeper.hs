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

import GateNetwork
import GateCRDT
import GateLDAP

getInitData :: IO (MVar State)
getInitData = do
       m <- newEmptyMVar
       putMVar m (State (Set [Tag "Tits McGee" "4848484" "d8f8c8a4-4671-11e4-8001-52540004b198"] []) (Cluster [] []) NotAMember)
       return m

main = do
        progName <- getProgName
        args <- getArgs
        case (length args) of
            2 -> do d <- getInitData
                    forkIO $ do fetchTagChanges d; return ()
                    (Just uid) <- U1.nextUUID
                    modifyMVar_ d (\s -> return $ changeMembership (addHost s (Host (args !! 0) (show uid))) Member)
                    s <- listenOn $ PortNumber $ fromIntegral $ read (args !! 1)
                    netloop s d (args !! 0) (args !! 1)
                    return ()
            3 -> do d <- getInitData
                    forkIO $ do askForNodes (args !! 2) (args !! 1)
                                modifyMVar_ d (\s -> return $ changeMembership s Joining)
                    s <- listenOn $ PortNumber $ fromIntegral $ read (args !! 1)
                    netloop s d (args !! 0) (args !! 1)
                    return ()
            _ -> putStrLn $ "Usage: " ++ progName ++ " <hostname> <portnum> (<known host>)"
