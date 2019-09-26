module RiotAPI.Match where

import           Control.Lens
import           Data.Aeson
import           Data.Text     as T
import           Network.Wreq
import           RiotAPI.Types

matchAPIUrl :: String
matchAPIUrl = "https://ru.api.riotgames.com/lol/match/v4/matches/"

--riotUrlBuilder :: Region -> Endpoint
getMatchByEncryptedAccountId :: ApiKey -> EncryptedAccountId -> IO (Response Match)
getMatchByEncryptedAccountId key accountId = asJSON =<< get (mconcat [matchAPIUrl, "by-account/", accountId])

getMatchByMatchId :: ApiKey -> Integer -> IO (Response Match)
getMatchByMatchId key matchId = do
  let opts = defaults & param "api_key" .~ [key]
  asJSON =<< getWith opts (mconcat [matchAPIUrl, show matchId])
