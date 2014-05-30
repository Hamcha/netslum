module Paradise.Client
( VesselID
, PlayerData (..)
) where

--import qualified Paradise.API as API

type VesselID = Int

data PlayerData = PlayerData { vessel :: VesselID
                             }