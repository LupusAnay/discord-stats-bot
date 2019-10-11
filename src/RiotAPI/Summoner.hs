module RiotAPI.Summoner where

import           Control.Lens
import           Data.Aeson
import           Data.Text              as T
import           GHC.Generics           (Generic)
import           Network.Wreq
import           RiotAPI.Data.Summoners
import           RiotAPI.Request
import           RiotAPI.Types


summonerBaseUrl :: String
summonerBaseUrl = "https://ru.api.riotgames.com/lol/summoner/v4/summoners/"

getSummonerByName :: ApiKey -> SummonerName -> IO (Response Summoner)
getSummonerByName key name = authorizedRequest key ["summoner/", "v4/", "summoners/", "by-name/", name]
