module Lib where

import           Control.Lens
import           Network.Wreq
import           RiotAPI.Match
import           RiotAPI.Summoner
import           Text.Pretty.Simple (pPrint)

key = "RGAPI-808acc5c-00ef-469c-a630-6ab7d1537d82"

someFunc :: IO ()
someFunc = do
  resp <- getSummonerByName key "lupusanay"
  let summoner = resp ^. responseBody
  let id = accountId summoner
  matchListResp <- getMatchListByEncryptedAccountId key id
  let matchList = matchListResp ^. responseBody
  let firstGameId = gameId $ matches matchList !! 1
  matchResp <- getMatchByMatchId key firstGameId
  let match = matchResp ^. responseBody
  pPrint match
