module RiotAPI.Data.Summoners where

import           Data.Aeson
import           GHC.Generics (Generic)

data Summoner =
  Summoner
    { profileIconId :: Int
    , name          :: String
    , puuid         :: String
    , summonerLevel :: Integer
    , revisionDate  :: Integer
    , id            :: String
    , accountId     :: String
    }
  deriving (Show, Generic, FromJSON)
