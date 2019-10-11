module RiotAPI.Request where

import           Control.Lens
import           Data.Aeson
import           Network.Wreq
import           RiotAPI.Types

authorizedRequest :: FromJSON a => ApiKey -> [String] -> IO (Response a)
authorizedRequest key uri = do
  let opts = defaults & param "api_key" .~ [key]
  asJSON =<< getWith opts (mconcat $ "https://ru.api.riotgames.com/lol/" : uri)
