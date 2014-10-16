{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Gatekeeper.Protobufs ( Msg ( Msg
                                  , msgusersadd
                                  , msgtagsadd
                                  , msgtuidsadd
                                  , msgusersdel
                                  , msgtagsdel
                                  , msgtuidsdel
                                  , msghostsadd
                                  , msghuidsadd
                                  , msghostsdel
                                  , msghuidsdel
                                  , msgclkuids
                                  , msgclkcnts
                                  )
                            , blobToMsg
                            , msgToBlob
                            , msgToAddTags
                            , msgToDelTags
                            , msgToAddHosts
                            , msgToDelHosts
                            , msgToVClocks
                            , newMsg
                            , stateToMsg
                            ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Hex as H
import qualified Data.Int as I
import qualified Data.ProtocolBuffers as P
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as SP
import qualified Data.Text as T
import qualified Data.Int as I
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
data Msg = Msg { msgusersadd  :: P.Repeated 1  (P.Value T.Text)
               , msgtagsadd   :: P.Repeated 2  (P.Value T.Text)
               , msgtuidsadd  :: P.Repeated 3  (P.Value T.Text)
               , msgusersdel  :: P.Repeated 4  (P.Value T.Text)
               , msgtagsdel   :: P.Repeated 5  (P.Value T.Text)
               , msgtuidsdel  :: P.Repeated 6  (P.Value T.Text)
               , msghostsadd  :: P.Repeated 7  (P.Value T.Text)
               , msghuidsadd  :: P.Repeated 8  (P.Value T.Text)
               , msghostsdel  :: P.Repeated 9  (P.Value T.Text)
               , msghuidsdel  :: P.Repeated 10 (P.Value T.Text)
               , msgclkuids   :: P.Repeated 11 (P.Value T.Text)
               , msgclkcnts   :: P.Repeated 12 (P.Value I.Int64)
               } deriving (Generic,Show)

instance P.Encode Msg
instance P.Decode Msg

blobToMsg :: String -> Either String Msg
blobToMsg s = G.runGet P.decodeMessage =<< H.unhex (C.pack s)

msgToBlob :: Msg -> String
msgToBlob m = C.unpack $ fmap H.hex SP.runPut $ P.encodeMessage m

msgToAddTags :: Msg -> Maybe [Tag]
msgToAddTags msg = if ((length users) == (length tags)) && ((length tags) == (length uids))
                      then Just $ zipWith3 (\user tagid uid -> (Tag user tagid uid)) users tags uids
                      else Nothing
                      where users = map T.unpack $ P.getField $ msgusersadd msg
                            tags  = map T.unpack $ P.getField $ msgtagsadd  msg
                            uids  = map T.unpack $ P.getField $ msgtuidsadd msg

msgToDelTags :: Msg -> Maybe [Tag]
msgToDelTags msg = if ((length users) == (length tags)) && ((length tags) == (length uids))
                      then Just $ zipWith3 (\user tagid uid -> (Tag user tagid uid)) users tags uids
                      else Nothing
                      where users = map T.unpack $ P.getField $ msgusersdel msg
                            tags  = map T.unpack $ P.getField $ msgtagsdel  msg
                            uids  = map T.unpack $ P.getField $ msgtuidsdel msg

msgToAddHosts :: Msg -> Maybe [Host]
msgToAddHosts msg = if (length hosts) == (length uids)
                       then Just $ zipWith (\host uid -> (Host host uid)) hosts uids
                       else Nothing
                       where hosts = map T.unpack $ P.getField $ msghostsadd msg
                             uids  = map T.unpack $ P.getField $ msghuidsadd msg

msgToDelHosts :: Msg -> Maybe [Host]
msgToDelHosts msg = if (length hosts) == (length uids)
                       then Just $ zipWith (\host uid -> (Host host uid)) hosts uids
                       else Nothing
                       where hosts = map T.unpack $ P.getField $ msghostsdel msg
                             uids  = map T.unpack $ P.getField $ msghuidsdel msg

msgToVClocks :: Msg -> Maybe [HostClock]
msgToVClocks msg = if (length uids) == (length counts)
                      then Just $ zipWith (\uid count -> (HostClock uid count)) uids counts
                      else Nothing
                          where uids   = map T.unpack $ P.getField $ msgclkuids msg
                                counts = map fromIntegral $ P.getField $ msgclkcnts msg

newMsg :: [Tag] -> [Tag] -> [Host] -> [Host] -> [HostClock] -> Msg
newMsg ta td ha hd hc =
        Msg { msgusersadd = P.putField $ map (\(Tag user _ _)    -> T.pack user) ta
            , msgtagsadd  = P.putField $ map (\(Tag _ id _)      -> T.pack id)   ta
            , msgtuidsadd = P.putField $ map (\(Tag _ _ uid)     -> T.pack uid)  ta
            , msgusersdel = P.putField $ map (\(Tag user _ _)    -> T.pack user) td
            , msgtagsdel  = P.putField $ map (\(Tag _ id _)      -> T.pack id)   td
            , msgtuidsdel = P.putField $ map (\(Tag _ _ uid)     -> T.pack uid)  td
            , msghostsadd = P.putField $ map (\(Host host _)     -> T.pack host) ha
            , msghuidsadd = P.putField $ map (\(Host _ uid)      -> T.pack uid)  ha
            , msghostsdel = P.putField $ map (\(Host host _)     -> T.pack host) hd
            , msghuidsdel = P.putField $ map (\(Host _ uid)      -> T.pack uid)  hd
            , msgclkuids  = P.putField $ map (\(HostClock uid _) -> T.pack uid)  hc
            , msgclkcnts  = P.putField $ map (\(HostClock _ cnt) -> (fromIntegral cnt) :: I.Int64)  hc
            }

stateToMsg :: State -> Msg
stateToMsg (State (Set ta td) (Cluster ha hd) (NetState _ _ hc _)) = newMsg ta td ha hd hc
