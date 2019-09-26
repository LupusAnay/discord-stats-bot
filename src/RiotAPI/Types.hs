module RiotAPI.Types where

import           Data.Aeson
import           Data.Map     as M
import           Data.Text    as T
import           GHC.Generics (Generic)

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

data Lane
  = Mid
  | Middle
  | Top
  | Jungle
  | Bot
  | Bottom
  deriving (Show, Generic)

data Role
  = Duo
  | None
  | Solo
  | DuoCarry
  | DuoSupport
  deriving (Show, Generic)

data Win
  = Fail
  | Win
  deriving (Show, Generic)

data Tier
  = Challenger
  | Master
  | Diamond
  | Platinum
  | Gold
  | Silver
  | Bronze
  | Unranked
  deriving (Show, Generic)

data ParticipantIdentity =
  ParticipantIdentity
    { player        :: Player
    , participantId :: Int
    }
  deriving (Show, Generic)

data ParticipantStats =
  ParticipantStats
    { firstBloodAssist                :: Bool
    , visionScore                     :: Integer
    , magicDamageDealtToChampions     :: Integer
    , damageDealtToObjectives         :: Integer
    , totalTimeCrowdControlDealt      :: Int
    , longestTimeSpentLiving          :: Int
    , perk1Var1                       :: Int
    , perk1Var3                       :: Int
    , perk1Var2                       :: Int
    , tripleKills                     :: Int
    , perk3Var3                       :: Int
    , nodeNeutralizeAssist            :: Int
    , perk3Var2                       :: Int
    , playerScore9                    :: Int
    , playerScore8                    :: Int
    , kills                           :: Int
    , playerScore1                    :: Int
    , playerScore0                    :: Int
    , playerScore3                    :: Int
    , playerScore2                    :: Int
    , playerScore5                    :: Int
    , playerScore4                    :: Int
    , playerScore7                    :: Int
    , playerScore6                    :: Int
    , perk5Var1                       :: Int
    , perk5Var3                       :: Int
    , perk5Var2                       :: Int
    , totalScoreRank                  :: Int
    , neutralMinionsKilled            :: Int
    , damageDealtToTurrets            :: Integer
    , physicalDamageDealtToChampions  :: Integer
    , nodeCapture                     :: Int
    , largestMultiKill                :: Int
    , perk2Var2                       :: Int
    , perk2Var3                       :: Int
    , totalUnitsHealed                :: Int
    , perk2Var1                       :: Int
    , perk4Var1                       :: Int
    , perk4Var2                       :: Int
    , perk4Var3                       :: Int
    , wardsKilled                     :: Int
    , largestCriticalStrike           :: Int
    , largestKillingSpree             :: Int
    , quadraKills                     :: Int
    , teamObjective                   :: Int
    , magicDamageDealt                :: Integer
    , item2                           :: Int
    , item3                           :: Int
    , item0                           :: Int
    , neutralMinionsKilledTeamJungle  :: Int
    , item6                           :: Int
    , item4                           :: Int
    , item5                           :: Int
    , perk1                           :: Int
    , perk0                           :: Int
    , perk3                           :: Int
    , perk2                           :: Int
    , perk5                           :: Int
    , perk4                           :: Int
    , perk3Var1                       :: Int
    , damageSelfMitigated             :: Integer
    , magicalDamageTaken              :: Integer
    , firstInhibitorKill              :: Bool
    , trueDamageTaken                 :: Integer
    , nodeNeutralize                  :: Int
    , assists                         :: Int
    , combatPlayerScore               :: Int
    , perkPrimaryStyle                :: Int
    , goldSpent                       :: Int
    , trueDamageDealt                 :: Integer
    , participantId                   :: Int
    , totalDamageTaken                :: Integer
    , physicalDamageDealt             :: Integer
    , sightWardsBoughtInGame          :: Int
    , totalDamageDealtToChampions     :: Integer
    , physicalDamageTaken             :: Integer
    , totalPlayerScore                :: Int
    , win                             :: Bool
    , objectivePlayerScore            :: Int
    , totalDamageDealt                :: Integer
    , item1                           :: Int
    , neutralMinionsKilledEnemyJungle :: Int
    , deaths                          :: Int
    , wardsPlaced                     :: Int
    , perkSubStyle                    :: Int
    , turretKills                     :: Int
    , firstBloodKill                  :: Bool
    , trueDamageDealtToChampions      :: Integer
    , goldEarned                      :: Int
    , killingSprees                   :: Int
    , unrealKills                     :: Int
    , altarsCaptured                  :: Int
    , firstTowerAssist                :: Bool
    , firstTowerKill                  :: Bool
    , champLevel                      :: Int
    , doubleKills                     :: Int
    , nodeCaptureAssist               :: Int
    , inhibitorKills                  :: Int
    , firstInhibitorAssist            :: Bool
    , perk0Var1                       :: Int
    , perk0Var2                       :: Int
    , perk0Var3                       :: Int
    , visionWardsBoughtInGame         :: Int
    , altarsNeutralized               :: Int
    , pentaKills                      :: Int
    , totalHeal                       :: Integer
    , totalMinionsKilled              :: Int
    , timeCCingOthers                 :: Integer
    }
  deriving (Show, Generic)

data Player =
  Player
    { currentPlatformId :: String
    , summonerName      :: String
    , matchHistoryUri   :: String
    , platformId        :: String
    , currentAccountId  :: String
    , profileIcon       :: Int
    , summonerId        :: String
    , accountId         :: String
    }
  deriving (Show, Generic)

data Ban =
  Ban
    { pickTurn   :: Int
    , championId :: Int
    }
  deriving (Show, Generic)

data Team =
  Team
    { firstDragon          :: Bool
    , firstInhibitor       :: Bool
    , bans                 :: [Ban]
    , baronKills           :: Int
    , firstRiftHerald      :: Bool
    , firstBaron           :: Bool
    , riftHeraldKills      :: Int
    , firstBlood           :: Bool
    , teamId               :: Int
    , firstTower           :: Bool
    , vilemawKills         :: Int
    , inhibitorKills       :: Int
    , towerKills           :: Int
    , dominionVictoryScore :: Int
    , win                  :: Win
    , dragonKills          :: Int
    }
  deriving (Show, Generic)

data Rune =
  Rune
    { runeId :: Int
    , rank   :: Int
    }
  deriving (Show, Generic)

data ParticipantTimeline =
  ParticipantTimeline
    { lane                        :: Lane
    , participantId               :: Int
    , csDiffPerMinDeltas          :: M.Map String Double
    , goldPerMinDeltas            :: M.Map String Double
    , xpDiffPerMinDeltas          :: M.Map String Double
    , creepsPerMinDeltas          :: M.Map String Double
    , xpPerMinDeltas              :: M.Map String Double
    , role                        :: Role
    , damageTakenDiffPerMinDeltas :: M.Map String Double
    , damageTakenPerMinDeltas     :: M.Map String Double
    }
  deriving (Show, Generic)

data Participant =
  Participant
    { stats                     :: ParticipantStats
    , participantId             :: Int
    , runes                     :: [Rune]
    , timeline                  :: [ParticipantTimeline]
    , teamId                    :: Int
    , spell2Id                  :: Int
    , highestAchievedSeasonTier :: Tier
    , spell1Id                  :: Int
    , championId                :: Int
    }
  deriving (Show, Generic)

data Match =
  Match
    { seasonId              :: Int
    , queueId               :: Int
    , gameId                :: Int
    , participantIdentities :: [ParticipantIdentity]
    , gameVersion           :: String
    , platformId            :: String
    , gameMode              :: String
    , mapId                 :: Int
    , gameType              :: String
    , teams                 :: [Team]
    , participants          :: [Participant]
    , gameDuration          :: Integer
    , gameCreation          :: Integer
    }
  deriving (Show, Generic)

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
  deriving (Show, Generic)

type ApiKey = T.Text

type EncryptedAccountId = String

type SummonerName = String

instance FromJSON Match

instance FromJSON ParticipantIdentity

instance FromJSON Player

instance FromJSON Team

instance FromJSON Participant

instance FromJSON Ban

instance FromJSON ParticipantStats

instance FromJSON Win

instance FromJSON Rune

instance FromJSON ParticipantTimeline

instance FromJSON Lane

instance FromJSON Tier

instance FromJSON Role

instance FromJSON Summoner