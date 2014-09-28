{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Gatekeeper.Protobufs ( Msg ( Msg
                                  , msgoper
                                  , msgusers
                                  , msgtags
                                  , msgtuids
                                  , msghosts
                                  , msghuids
                                  )
                            , blobToMsg
                            , msgToBlob
                            , msgToTags
                            , msgToHosts
                            , addTagsMsg
                            , delTagsMsg
                            , addHostsMsg
                            , delHostsMsg
                            , reqNodesMsg
                            ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Hex as H
import qualified Data.Int as I
import qualified Data.ProtocolBuffers as P
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as SP
import qualified Data.Text as T
import GHC.Generics (Generic)

import Gatekeeper.CRDT

data Msg = Msg { msgoper   :: P.Required 1 (P.Value I.Int64)
               , msgusers  :: P.Repeated 2 (P.Value T.Text)
               , msgtags   :: P.Repeated 3 (P.Value T.Text)
               , msgtuids  :: P.Repeated 4 (P.Value T.Text)
               , msghosts  :: P.Repeated 5 (P.Value T.Text)
               , msghuids  :: P.Repeated 6 (P.Value T.Text)
               } deriving (Generic,Show)

instance P.Encode Msg
instance P.Decode Msg

blobToMsg :: String -> Either String Msg
blobToMsg s = G.runGet P.decodeMessage =<< H.unhex (C.pack s)

msgToBlob :: Msg -> String
msgToBlob m = C.unpack $ fmap H.hex SP.runPut $ P.encodeMessage m

msgToTags :: Msg -> Maybe [Tag]
msgToTags msg = if ((length users) == (length tags)) && ((length tags) == (length uids))
                    then Just $ zipWith3 (\user tagid uid -> (Tag user tagid uid)) users tags uids
                    else Nothing
                    where users = map T.unpack $ P.getField $ msgusers msg
                          tags  = map T.unpack $ P.getField $ msgtags  msg
                          uids  = map T.unpack $ P.getField $ msgtuids msg

msgToHosts :: Msg -> Maybe [Host]
msgToHosts msg = if (length hosts) == (length uids)
                    then Just $ zipWith (\host uid -> (Host host uid)) hosts uids
                    else Nothing
                    where hosts = map T.unpack $ P.getField $ msghosts msg
                          uids  = map T.unpack $ P.getField $ msghuids msg

addTagsMsg :: [Tag] -> Msg
addTagsMsg tags = 
        Msg { msgoper  = P.putField 0
            , msgusers = P.putField $ map (\(Tag user _ _) -> T.pack user) tags
            , msgtags  = P.putField $ map (\(Tag _ id _) -> T.pack id) tags
            , msgtuids = P.putField $ map (\(Tag _ _ uid) -> T.pack uid) tags
            , msghosts = P.putField []
            , msghuids = P.putField []
            }

delTagsMsg :: [Tag] -> Msg
delTagsMsg tags = 
        Msg { msgoper  = P.putField 1
            , msgusers = P.putField $ map (\(Tag user _ _) -> T.pack user) tags
            , msgtags  = P.putField $ map (\(Tag _ id _) -> T.pack id) tags
            , msgtuids = P.putField $ map (\(Tag _ _ uid) -> T.pack uid) tags
            , msghosts = P.putField []
            , msghuids = P.putField []
            }

addHostsMsg :: [Host] -> Msg
addHostsMsg hosts = 
        Msg { msgoper  = P.putField 2
            , msgtags  = P.putField []
            , msgusers = P.putField []
            , msgtuids = P.putField []
            , msghosts = P.putField $ map (\(Host h _) -> T.pack h) hosts
            , msghuids = P.putField $ map (\(Host _ u) -> T.pack u) hosts
            }

delHostsMsg :: [Host] -> Msg
delHostsMsg hosts = 
        Msg { msgoper  = P.putField 3
            , msgtags  = P.putField []
            , msgusers = P.putField []
            , msgtuids = P.putField []
            , msghosts = P.putField $ map (\(Host h _) -> T.pack h) hosts
            , msghuids = P.putField $ map (\(Host _ u) -> T.pack u) hosts
            }

reqNodesMsg :: Msg
reqNodesMsg = 
        Msg { msgoper  = P.putField 4
            , msgtags  = P.putField []
            , msgusers = P.putField []
            , msgtuids = P.putField []
            , msghosts = P.putField []
            , msghuids = P.putField []
            }
