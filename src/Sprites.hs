module Sprites where

import Data.Monoid
import Graphics.Gloss

import Types


letterPicture :: String -> Picture
letterPicture s = translate (-5) (-6)
                $ scale 0.12 0.12
                $ text s


emptyPicture :: Picture
emptyPicture = blank

floorPicture :: Picture
floorPicture = blank

wallPicture :: Picture
wallPicture = circle 10

startPicture :: Picture
startPicture = circle 10
            <> letterPicture "S"

goalPicture :: Picture
goalPicture = circle 10
           <> letterPicture "G"

playerPicture :: Picture
playerPicture = color white (thickCircle 0 20)
             <> circle 10
             <> rotate 90 (letterPicture ":)")

lockedDoorPicture :: Picture
lockedDoorPicture = unlockedDoorPicture
                 <> rotate (-90) (letterPicture "-")

unlockedDoorPicture :: Picture
unlockedDoorPicture = circle 10
                   <> rotate (-90) (letterPicture "D")

keyPicture :: Picture
keyPicture = circle 10
          <> letterPicture "K"


tilePicture :: Tile -> Picture
tilePicture Start        = startPicture
tilePicture Goal         = goalPicture
tilePicture Empty        = emptyPicture
tilePicture Floor        = floorPicture
tilePicture Wall         = wallPicture
tilePicture LockedDoor   = lockedDoorPicture
tilePicture UnlockedDoor = unlockedDoorPicture
tilePicture (Key _)      = keyPicture
