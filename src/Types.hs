module Types where

import Vec2d


gameTitle :: String
gameTitle = "More of the Same"


type LevelNumber = Int

data Tile = Start
          | Goal
          | Floor
          | Wall
          | LockedDoor
          | UnlockedDoor
          | Key LevelNumber
  deriving (Show, Eq)

type Stage = [[Tile]]

type LevelChanges = [(V Int, Tile)]

type TilePos = V Int
type ScreenPos = V Float

data PlayerGraphics = PlayerGraphics
  { gPlayerVisible      :: Bool
  , gPlayerScreenPos    :: ScreenPos
  } deriving (Show, Eq)

data GameState = GameState
  { gLevelNumber        :: LevelNumber
  , gStage              :: Stage
  , gPlayerTilePos      :: TilePos
  , gPlayerGraphics     :: PlayerGraphics
  , gAccumulatedChanges :: [LevelChanges]
  , gDebugMessages      :: [String]
  } deriving (Show, Eq)

startTilePos :: TilePos
startTilePos = 0

goalTilePos :: TilePos
goalTilePos = 4

startScreenPos :: ScreenPos
startScreenPos = fmap fromIntegral startTilePos

goalScreenPos :: ScreenPos
goalScreenPos = fmap fromIntegral goalTilePos

initialPlayerGraphics :: PlayerGraphics
initialPlayerGraphics = PlayerGraphics True startScreenPos

initialStage :: [[Tile]]
initialStage = [[Start, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Goal]]
