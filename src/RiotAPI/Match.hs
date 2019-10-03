module RiotAPI.Match where

import           Control.Lens
import           Data.Aeson
import           Data.Text               as T
import           GHC.Generics            (Generic)
import           Network.Wreq
import           RiotAPI.Data.Matches
import           RiotAPI.Data.MatchLists
import           RiotAPI.Types

matchAPIUrl :: String
matchAPIUrl = "https://ru.api.riotgames.com/lol/match/v4/matches/"

matchListAPIUrl = "https://ru.api.riotgames.com/lol/match/v4/matchlists/"

getMatchListByEncryptedAccountId :: ApiKey -> EncryptedAccountId -> IO (Response MatchList)
getMatchListByEncryptedAccountId key accountId = do
  let opts = defaults & param "api_key" .~ [key]
  asJSON =<< getWith opts (mconcat [matchListAPIUrl, "by-account/", accountId])

getMatchByMatchId :: ApiKey -> Integer -> IO (Response Match)
getMatchByMatchId key matchId = do
  let opts = defaults & param "api_key" .~ [key]
  asJSON =<< getWith opts (mconcat [matchAPIUrl, show matchId])
