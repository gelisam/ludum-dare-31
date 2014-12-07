module Sprites where

import Data.Monoid
import Graphics.Gloss

import Types


letterPicture :: String -> Picture
letterPicture s = translate (-15) (-18)
                $ scale 0.36 0.36
                $ text s


emptyPicture :: Picture
emptyPicture = blank

floorPicture :: Picture
floorPicture = blank

invisibleWallPicture :: Picture
invisibleWallPicture = blank

wallPicture :: Picture
wallPicture = circle 30

startPicture :: Picture
startPicture = circle 30
            <> letterPicture "S"

goalPicture :: Picture
goalPicture = circle 30
           <> letterPicture "G"

playerPicture :: Picture
playerPicture = color white (thickCircle 0 60)
             <> circle 30
             <> rotate 90 (letterPicture ":)")

lockedDoorPicture :: Picture
lockedDoorPicture = unlockedDoorPicture
                 <> rotate (-90) (letterPicture "-")

unlockedDoorPicture :: Picture
unlockedDoorPicture = circle 30
                   <> rotate (-90) (letterPicture "D")

keyPicture :: Picture
keyPicture = circle 30
          <> letterPicture "K"


tilePicture :: Tile -> Picture
tilePicture Start        = startPicture
tilePicture Goal         = goalPicture
tilePicture Empty        = emptyPicture
tilePicture Floor        = floorPicture
tilePicture Wall         = wallPicture
tilePicture XWall        = invisibleWallPicture
tilePicture LockedDoor   = lockedDoorPicture
tilePicture UnlockedDoor = unlockedDoorPicture
tilePicture (Key _)      = keyPicture
