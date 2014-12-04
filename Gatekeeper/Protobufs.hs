{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Gatekeeper.Protobufs ( Msg ( Msg )
                            , blobToMsg
                            , msgToBlob
                            , msgToAddTags
                            , msgToDelTags
                            , msgToAddHosts
                            , msgToDelHosts
                            , msgToVClocks
                            , newMsg
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

--Msg contains arrays which represent two [Tag]s, two [Host]s, and one 
--[HostClock]. 
--msgusersadd, msgtagsadd, and msgtuidsadd represent a [Tag] containing elements
--to be appended to the added set of tags.
--msgusersdel, msgtagsdel, and msgtuidsdel represent a [Tag] containing elements
--to be appended to the removed set of tags.
--msghostsadd and msghuidsadd represent a [Host], along with their del variants.
--The [HostClock] is represented by msgclkuids and msgclkcnts

data MsgHostClock = MsgHostClock { msghcuid :: P.Required 1 (P.Value T.Text)
                                 , msghccnt :: P.Required 2 (P.Value I.Int64)
                                 } deriving (Generic,Show)
instance P.Encode MsgHostClock
instance P.Decode MsgHostClock

data MsgTag = MsgTag { msguser :: P.Required 1 (P.Value T.Text)
                     , msgtid  :: P.Required 2 (P.Value T.Text)
                     , msgtuid :: P.Required 3 (P.Value T.Text)
                     , msgthc  :: P.Required 4 (P.Message MsgHostClock)
                     } deriving (Generic,Show)
instance P.Encode MsgTag
instance P.Decode MsgTag

data MsgHost = MsgHost { msghname :: P.Required 1 (P.Value T.Text)
                       , msghuid  :: P.Required 2 (P.Value T.Text)
                       , msghhc   :: P.Required 3 (P.Message MsgHostClock)
                       } deriving (Generic,Show)
instance P.Encode MsgHost
instance P.Decode MsgHost

data Msg = Msg { msgAddTags :: P.Repeated 1 (P.Message MsgTag)
               , msgDelTags :: P.Repeated 2 (P.Message MsgTag)
               , msgAddHsts :: P.Repeated 3 (P.Message MsgHost)
               , msgDelHsts :: P.Repeated 4 (P.Message MsgHost)
               , msgVClocks :: P.Repeated 5 (P.Message MsgHostClock)
               } deriving (Generic,Show)
instance P.Encode Msg
instance P.Decode Msg

hcToMsgHc :: HostClock -> MsgHostClock
hcToMsgHc (HostClock vu vc) = 
        MsgHostClock { msghcuid = P.putField $ T.pack vu
                     , msghccnt = P.putField (fromIntegral vc :: I.Int64)
                     }

tagToMsgTag :: Tag -> MsgTag
tagToMsgTag (Tag n i u v) = 
        MsgTag { msguser = P.putField $ T.pack n
               , msgtid  = P.putField $ T.pack i
               , msgtuid = P.putField $ T.pack u
               , msgthc  = P.putField $ hcToMsgHc v
               }

hostToMsgHost :: Host -> MsgHost
hostToMsgHost (Host h u v) = 
        MsgHost { msghname = P.putField $ T.pack h
                , msghuid  = P.putField $ T.pack u
                , msghhc   = P.putField $ hcToMsgHc v
                }

blobToMsg :: String -> Either String Msg
blobToMsg s = G.runGet P.decodeMessage =<< H.unhex (C.pack s)

msgToBlob :: Msg -> String
msgToBlob m = C.unpack $ fmap H.hex SP.runPut $ P.encodeMessage m

msgConvertTags :: MsgTag -> Tag
msgConvertTags msgTag = let user = T.unpack $ P.getField $ msguser msgTag
                            tid  = T.unpack $ P.getField $ msgtid  msgTag
                            uid  = T.unpack $ P.getField $ msgtuid msgTag
                            msgv = P.getField $ msgthc msgTag
                            vuid = T.unpack $ P.getField $ msghcuid msgv
                            vcnt = fromIntegral $ P.getField $ msghccnt msgv
                        in Tag user tid uid (HostClock vuid vcnt)

msgConvertHosts :: MsgHost -> Host
msgConvertHosts msgHost = let hst  = T.unpack $ P.getField $ msghname msgHost
                              uid  = T.unpack $ P.getField $ msghuid  msgHost
                              msgv = P.getField $ msghhc msgHost
                              vuid = T.unpack $ P.getField $ msghcuid msgv
                              vcnt = fromIntegral $ P.getField $ msghccnt msgv
                          in Host hst uid (HostClock vuid vcnt)

msgToAddTags :: Msg -> [Tag]
msgToAddTags msg = map msgConvertTags toadd
                      where toadd = P.getField $ msgAddTags msg

msgToDelTags :: Msg -> [Tag]
msgToDelTags msg = map msgConvertTags todel
                      where todel = P.getField $ msgDelTags msg

msgToAddHosts :: Msg -> [Host]
msgToAddHosts msg = map msgConvertHosts toadd
                      where toadd = P.getField $ msgAddHsts msg

msgToDelHosts :: Msg -> [Host]
msgToDelHosts msg = map msgConvertHosts todel
                      where todel = P.getField $ msgDelHsts msg

msgToVClocks :: Msg -> [HostClock]
msgToVClocks msg = map (\msgHostClock -> let vuid = T.unpack $ P.getField $ msghcuid msgHostClock
                                             vcnt = fromIntegral $ P.getField $ msghccnt msgHostClock
                                         in HostClock vuid vcnt) toadd
                      where toadd = P.getField $ msgVClocks msg

newMsg :: [Tag] -> [Tag] -> [Host] -> [Host] -> [HostClock] -> Msg
newMsg ta td ha hd hc =
        Msg { msgAddTags = P.putField $ map tagToMsgTag ta
            , msgDelTags = P.putField $ map tagToMsgTag td
            , msgAddHsts = P.putField $ map hostToMsgHost ha
            , msgDelHsts = P.putField $ map hostToMsgHost hd
            , msgVClocks = P.putField $ map hcToMsgHc hc
            }
