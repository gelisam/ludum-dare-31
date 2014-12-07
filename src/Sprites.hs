module Sprites where

import Control.Applicative
import Data.Monoid
import Graphics.Gloss
import System.FilePath

import Graphics.Gloss.Extra
import Types


data Sprites = Sprites
  { floorSprite        :: Picture
  , wallSprite         :: Picture
  , startSprite        :: Picture
  , goalSprite         :: Picture
  , playerSprite       :: Picture
  , lockedDoorSprite   :: Picture
  , unlockedDoorSprite :: Picture
  , keySprite          :: Picture
  }

loadSprites :: FilePath -> IO Sprites
loadSprites images = Sprites <$> pure floorPicture
                             <*> pure wallPicture
                             <*> pure startPicture
                             <*> pure goalPicture
                             <*> loadSprite (images </> "player.bmp")
                             <*> pure lockedDoorPicture
                             <*> pure unlockedDoorPicture
                             <*> pure keyPicture

loadSprite :: FilePath -> IO Picture
loadSprite = fmap (scale 6 6) . loadBMP

renderTile :: Sprites -> Tile -> Picture
renderTile = flip go
  where
    go Start        = startSprite
    go Goal         = goalSprite
    go Empty        = const blank
    go Floor        = floorSprite
    go Wall         = wallSprite
    go XWall        = const blank
    go LockedDoor   = lockedDoorSprite
    go UnlockedDoor = unlockedDoorSprite
    go (Key _)      = keySprite

renderPlayerSprite :: Sprites -> Picture
renderPlayerSprite = playerSprite


letterPicture :: String -> Picture
letterPicture s = translate (-15) (-18)
                $ scale 0.36 0.36
                $ blackText s


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
