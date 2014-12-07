module LevelData where

import Control.Applicative
import Data.Array
import Data.List

import Types
import Vec2d


gameTitle :: String
gameTitle = "I've Seen This Room Before"


roomPadding :: V Int
roomPadding = V 14 9

padTilePos :: TilePos -> TilePos
padTilePos tilePos = tilePos + roomPadding + 1

startTilePos :: TilePos
startTilePos = padTilePos (V 0 5)

goalTilePos :: TilePos
goalTilePos = padTilePos (V 4 1)

startScreenPos :: ScreenPos
startScreenPos = fmap fromIntegral startTilePos

goalScreenPos :: ScreenPos
goalScreenPos = fmap fromIntegral goalTilePos

initialPlayerGraphics :: PlayerGraphics
initialPlayerGraphics = PlayerGraphics True startScreenPos

initialStage :: Stage
initialStage = listArray (0, maxIndex)
             $ concat
             $ transpose
             $ reverse
             $ padRoom
             $ smallRoom
  where
    padRoom :: [[Tile]] -> [[Tile]]
    padRoom rows = [borderRow] ++ extraRows ++ paddedRows ++ extraRows ++ [borderRow]
      where
        borderRow = replicate w Goal
        extraRow = [Goal] ++ replicate (w-2) Empty ++ [Goal]
        extraRows = replicate yPadding extraRow
        
        leftPadding = [Goal] ++ replicate xPadding Empty
        rightPadding = reverse leftPadding
        padRow row = leftPadding ++ row ++ rightPadding
        
        paddedRows = padRow <$> rows
    
    w0, h0 :: Int
    w0 = length (head smallRoom)
    h0 = length smallRoom
    
    (xPadding, yPadding) = runV roomPadding
    
    w, h :: Int
    V w h = V w0 h0 + 2 * V xPadding yPadding + 2
    
    maxIndex :: TilePos
    maxIndex = V (w-1) (h-1)
    
    
    smallRoom :: [[Tile]]
    smallRoom = [[Wall , Wall , Wall , Wall , Wall]   -- 6
                ,[Start, Floor, Floor, Floor, Wall]   -- 5
                ,[Wall , Floor, Floor, Floor, Wall]   -- 4
                ,[Wall , Floor, Floor, Floor, Wall]   -- 3
                ,[Wall , Floor, Floor, Floor, Wall]   -- 2
                ,[Wall , Floor, Floor, Floor, Goal]   -- 1
                ,[Wall , Wall , Wall , Wall , Wall]]  -- 0
            --     0      1      2      3      4

levelData :: [LevelChanges]
levelData = (fmap.fmap) padData smallLevelData
  where
    smallLevelData = [[(V 2 2, Wall)]
                     ,[(V 2 1, LockedDoor)]
                     ,[(V 2 4, Key 0)]
                     ,[(V 1 1, LockedDoor)]
                     ,[(V 3 2, Wall)]
                     ]
    
    padData :: LevelChange -> LevelChange
    padData (tilePos, tile) = (padTilePos tilePos, tile)

lastLevel :: LevelNumber
lastLevel = length levelData
