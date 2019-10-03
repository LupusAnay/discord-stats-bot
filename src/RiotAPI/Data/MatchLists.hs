module RiotAPI.Data.MatchLists where

import           Data.Aeson
import           GHC.Generics (Generic)

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