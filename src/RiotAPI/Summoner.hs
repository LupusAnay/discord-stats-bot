module RiotAPI.Summoner where

import           Control.Lens
import           Data.Aeson
import           Data.Text     as T
import           GHC.Generics  (Generic)
import           Network.Wreq
import           RiotAPI.Types

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

summonerBaseUrl :: String
summonerBaseUrl = "https://ru.api.riotgames.com/lol/summoner/v4/summoners/"

getSummonerByName :: ApiKey -> SummonerName -> IO (Response Summoner)
getSummonerByName key name = do
  let opts = defaults & param "api_key" .~ [key]
  asJSON =<< getWith opts (mconcat [summonerBaseUrl, "by-name/", name])
