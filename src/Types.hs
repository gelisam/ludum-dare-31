module Types where

import Vec2d


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

data GameState = GameState
  { gLevelNumber        :: LevelNumber
  , gStage              :: Stage
  , gPlayerTilePos      :: TilePos
  , gPlayerScreenPos    :: ScreenPos
  , gAccumulatedChanges :: [LevelChanges]
  , gDebugMessages      :: [String]
  } deriving (Show, Eq)

startPosition :: TilePos
startPosition = 0

goalPosition :: TilePos
goalPosition = 4

initialStage :: [[Tile]]
initialStage = [[Start, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Goal]]
