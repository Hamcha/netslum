module Paradise.Client
( VesselID
, PlayerData (..)
) where

import qualified Data.ByteString.Char8 as B

type VesselID = B.ByteString

data PlayerData = PlayerData { vessel   :: VesselID
                             , position :: VesselID
                             }