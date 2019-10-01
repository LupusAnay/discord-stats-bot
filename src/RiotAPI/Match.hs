module RiotAPI.Match where

import           Control.Lens
import           Data.Aeson
import           Data.Text     as T
import           Network.Wreq
import           RiotAPI.Types
import GHC.Generics (Generic)

data MatchList =
  MatchList
    { matches    :: [MatchListItem]
    , totalGames :: Int
    , startIndex :: Int
    , endIndex   :: Int
    }
  deriving (Show, Generic, FromJSON)

data MatchListItem =
  MatchListItem
    { lane       :: String
    , gameId     :: Integer
    , champion   :: Int
    , platformId :: String
    , season     :: Int
    , queue      :: Int
    , role       :: String
    , timestamp  :: Integer
    }
  deriving (Show, Generic, FromJSON)

matchAPIUrl :: String
matchAPIUrl = "https://ru.api.riotgames.com/lol/match/v4/matches/"

matchListAPIUrl = "https://ru.api.riotgames.com/lol/match/v4/matchlists/"

--riotUrlBuilder :: Region -> Endpoint
getMatchListByEncryptedAccountId :: ApiKey -> EncryptedAccountId -> IO (Response MatchList)
getMatchListByEncryptedAccountId key accountId = do
  let opts = defaults & param "api_key" .~ [key]
  asJSON =<< getWith opts (mconcat [matchListAPIUrl, "by-account/", accountId])

getMatchByMatchId :: ApiKey -> Integer -> IO (Response Match)
getMatchByMatchId key matchId = do
  let opts = defaults & param "api_key" .~ [key]
  asJSON =<< getWith opts (mconcat [matchAPIUrl, show matchId])
