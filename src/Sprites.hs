module Sprites where

import Data.Monoid
import Graphics.Gloss


letterPicture :: String -> Picture
letterPicture s = translate (-5) (-6)
                $ scale 0.12 0.12
                $ text s

startPicture :: Picture
startPicture = circle 10
            <> letterPicture "S"

goalPicture :: Picture
goalPicture = circle 10
           <> letterPicture "G"

playerPicture :: Picture
playerPicture = circle 10
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
