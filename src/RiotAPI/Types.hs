module RiotAPI.Types where

import           Data.Aeson
import           Data.Char           (toUpper)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

data Region
  = BR
  | EUNE
  | EUW
  | JP
  | KR
  | LAN
  | LAS
  | NA
  | OCE
  | TR
  | RU
  | PBE

instance Show Region where
  show BR   = "br1"
  show EUNE = "eun1"
  show EUW  = "euw1"
  show JP   = "jp1"
  show KR   = "kr"
  show LAN  = "la1"
  show LAS  = "la2"
  show NA   = "na1"
  show OCE  = "oc1"
  show TR   = "tr1"
  show RU   = "ru"
  show PBE  = "pbe1"

type ApiKey = T.Text

type EncryptedAccountId = String

type SummonerName = String
