module Sprites where

import Control.Applicative
import Control.Exception
import Data.Monoid
import Graphics.Gloss
import System.FilePath
import System.IO

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
loadSprites images = Sprites <$> loadSprite floorPicture (images </> "floor.bmp")
                             <*> loadSprite wallPicture (images </> "wall.bmp")
                             <*> pure startPicture
                             <*> pure goalPicture
                             <*> loadSprite playerPicture (images </> "player.bmp")
                             <*> pure lockedDoorPicture
                             <*> pure unlockedDoorPicture
                             <*> pure keyPicture

loadSprite :: Picture -> FilePath -> IO Picture
loadSprite fallbackPicture imagePath = catch (uscale 6 <$> loadBMP imagePath) rescue
  where
    rescue :: IOException -> IO Picture
    rescue err = do
      hPutStrLn stderr (show err)
      return fallbackPicture

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
                $ uscale 0.36
                $ blackText s


emptyPicture :: Picture
emptyPicture = blank

floorPicture :: Picture
floorPicture = blank

invisibleWallPicture :: Picture
invisibleWallPicture = blank

wallPicture :: Picture
wallPicture = blackCircle 30

startPicture :: Picture
startPicture = blackCircle 30
            <> letterPicture "S"

goalPicture :: Picture
goalPicture = blackCircle 30
           <> letterPicture "G"

playerPicture :: Picture
playerPicture = color white (thickCircle 0 60)
             <> blackCircle 30
             <> rotate 90 (letterPicture ":)")

lockedDoorPicture :: Picture
lockedDoorPicture = unlockedDoorPicture
                 <> rotate (-90) (letterPicture "-")

unlockedDoorPicture :: Picture
unlockedDoorPicture = blackCircle 30
                   <> rotate (-90) (letterPicture "D")

keyPicture :: Picture
keyPicture = blackCircle 30
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
