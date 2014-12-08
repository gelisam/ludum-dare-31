module LevelData where

import Control.Applicative
import Data.Array
import Data.List

import Types
import Vec2d


gameTitle :: String
gameTitle = "I've Seen This Room Before"


roomPadding :: V Int
roomPadding = V 2 1

padTilePos :: TilePos -> TilePos
padTilePos tilePos = tilePos + roomPadding + 1

startTilePos :: TilePos
startTilePos = padTilePos (V 1 5)

goalTilePos :: TilePos
goalTilePos = padTilePos (V 5 1)

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
    smallRoom = [[Empty, Wall , Wall , Wall , Wall , Wall, Empty]   -- 6
                ,[XWall, Start, Floor, Floor, Floor, Wall, Empty]   -- 5
                ,[Empty, Wall , Floor, Floor, Floor, Wall, Empty]   -- 4
                ,[Empty, Wall , Floor, Floor, Floor, Wall, Empty]   -- 3
                ,[Empty, Wall , Floor, Floor, Floor, Wall, Empty]   -- 2
                ,[Empty, Wall , Floor, Floor, Floor, Goal, XWall]   -- 1
                ,[Empty, Wall , Wall , Wall , Wall , Wall, Empty]]  -- 0
            --     0      1      2      3      4      5     6
    
    -- first challenge
    -- 
    -- [[Empty, Wall , Wall , Wall , Wall , Wall, Empty]   -- 6
    -- ,[XWall, Start, Floor, Floor, Floor, Wall, Empty]   -- 5
    -- ,[Empty, Wall , Floor, Key3 , Floor  Wall, Empty]   -- 4
    -- ,[Empty, Wall , Floor, Floor, Floor, Wall, Empty]   -- 3
    -- ,[Empty, Wall , Floor, Wall1, Wall5, Wall, Empty]   -- 2
    -- ,[Empty, Wall , Door4, Door2, Floor, Goal, XWall]   -- 1
    -- ,[Empty, Wall , Wall , Wall , Wall , Wall, Empty]]  -- 0
    --    0      1      2      3      4      5     6
    
    -- second challenge
    -- 
    -- [[Empty, Wall , Wall , Wall , Wall6, Wall, Empty]   -- 6
    -- ,[XWall, Start, Door7, Door2, Key  , Wall, Empty]   -- 5
    -- ,[Empty, Wall , Key4 , Floor, Wall1, Wall, Empty]   -- 4
    -- ,[Empty, Wall , Floor, Door3, Key5 , Wall, Empty]   -- 3
    -- ,[Empty, Wall , Door4, D3W7 , Wall , Wall, Empty]   -- 2
    -- ,[Empty, Wall , Door7, Door7, Door7, Goal, XWall]   -- 1
    -- ,[Empty, Wall , Wall , Wall , Wall , Wall, Empty]]  -- 0
    --    0      1      2      3      4      5     6

levelData :: [LevelDescription]
levelData = padData <$> smallLevelData
  where
    smallLevelData = [ LevelDescription [(V 3 2, Wall)]                  "A new wall appears!"    "(the wall disappears silently)"
                     , LevelDescription [(V 3 1, LockedDoor)]            "A new door appears!"    "(the door disappears silently)"
                     , LevelDescription [(V 3 4, Key 0)]                 "A key appears!"         "(the key disappears silently)"
                     , LevelDescription [(V 2 1, LockedDoor)]            "Another door appears!"  "(the door disappears silently)"
                     , LevelDescription [(V 4 2, Wall)]                  "Another wall appears!"  "(yes, better keep exploring)"
                     , LevelDescription [(V 4 4, Wall)]                  "You got it!"            "(where are you going?)"
                     , LevelDescription [(V 3 5, LockedDoor)
                                        ,(V 2 1, Floor)
                                        ,(V 3 1, Floor)
                                        ]                                "New day, new door."     "(puzzle reset)"
                     , LevelDescription [(V 3 3, LockedDoor)
                                        ,(V 3 2, LockedDoor)
                                        ]                                "A matching set!"        "(no more matching set)"
                     , LevelDescription [(V 2 2, LockedDoor)
                                        ,(V 2 5, Key 1)
                                        ]                                "A key, finally!"        "(no more keys)"
                     , LevelDescription [(V 4 3, Key 2)]                 "Another key."           "(another key disappears)"
                     , LevelDescription [(V 4 5, Key 3)]                 "Yay! I love keys :)"    "(one key disappears)"
                     , LevelDescription [(V 4 1, LockedDoor)
                                        ,(V 3 1, LockedDoor)
                                        ,(V 2 1, LockedDoor)
                                        ,(V 3 2, Wall)
                                        ]                                "So many doors..."       "(so many keys to find...)"
                     , LevelDescription [(V 5 1, Floor), (V 6 1, Empty)] "Freedom unlocked"       ""
                     ]
    
    padData :: LevelDescription -> LevelDescription
    padData level = level { lLevelChanges = fmap padChange (lLevelChanges level) }
    
    padChange :: LevelChange -> LevelChange
    padChange (tilePos, tile) = (padTilePos tilePos, tile)

lastLevel :: LevelNumber
lastLevel = length levelData
