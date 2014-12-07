module Sprites where

import Control.Applicative
import Control.Exception
import Data.Monoid
import Graphics.Gloss
import System.FilePath
import System.IO

import Graphics.Gloss.Extra
import Types


data Sprite = Sprite
  { basePicture :: Picture
  , topPicture :: Picture
  }

blankSprite :: Sprite
blankSprite = Sprite blank blank


data Sprites = Sprites
  { floorSprite        :: Sprite
  , wallSprite         :: Sprite
  , startSprite        :: Sprite
  , goalSprite         :: Sprite
  , playerSprite       :: Sprite
  , lockedDoorSprite   :: Sprite
  , unlockedDoorSprite :: Sprite
  , keySprite          :: Sprite
  , inventoryKeySprite :: Sprite
  }

loadSprites :: FilePath -> IO Sprites
loadSprites images = Sprites <$> loadSprite        floorPicture        (images </> "floor.bmp")
                             <*> loadSprite        wallPicture         (images </> "wall.bmp")
                             <*> loadLayeredSprite startPicture        (images </> "start.bmp")    (images </> "start-top.bmp")
                             <*> loadLayeredSprite goalPicture         (images </> "goal.bmp")     (images </> "goal-top.bmp")
                             <*> loadSprite        playerPicture       (images </> "player.bmp")
                             <*> loadSprite        lockedDoorPicture   (images </> "locked.bmp")
                             <*> loadLayeredSprite unlockedDoorPicture (images </> "unlocked.bmp") (images </> "unlocked-top.bmp")
                             <*> loadSprite        keyPicture          (images </> "key.bmp")
                             <*> loadSprite        keyPicture          (images </> "inventory-key.bmp")

loadPicture :: Picture -> FilePath -> IO Picture
loadPicture fallbackPicture imagePath = catch (uscale 6 <$> loadBMP imagePath) rescue
  where
    rescue :: IOException -> IO Picture
    rescue err = do
      hPutStrLn stderr (show err)
      return fallbackPicture

loadSprite :: Picture -> FilePath -> IO Sprite
loadSprite fallbackPicture imagePath = Sprite <$> loadPicture fallbackPicture imagePath
                                              <*> pure blank

loadLayeredSprite :: Picture -> FilePath -> FilePath -> IO Sprite
loadLayeredSprite fallbackPicture baseImagePath topImagePath = Sprite <$> loadPicture fallbackPicture baseImagePath
                                                                      <*> loadPicture blank topImagePath


tileSprite :: Sprites -> Tile -> Sprite
tileSprite = flip go
  where
    go Start        = startSprite
    go Goal         = goalSprite
    go Empty        = const blankSprite
    go Floor        = floorSprite
    go Wall         = wallSprite
    go XWall        = const blankSprite
    go LockedDoor   = lockedDoorSprite
    go UnlockedDoor = unlockedDoorSprite
    go (Key _)      = keySprite

renderTile :: Sprites -> Tile -> Picture
renderTile sprites = basePicture . tileSprite sprites

renderTileTop :: Sprites -> Tile -> Picture
renderTileTop sprites = topPicture . tileSprite sprites

renderPlayerSprite :: Sprites -> Picture
renderPlayerSprite = basePicture . playerSprite

renderInventoryKey :: Sprites -> KeyNumber -> Picture
renderInventoryKey sprites _ = basePicture (inventoryKeySprite sprites)


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
