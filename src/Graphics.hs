{-# LANGUAGE RecordWildCards #-}
module Graphics where

import Data.Monoid
import Graphics.Gloss
import Text.Printf

import Graphics.Gloss.Extra
import Sprites
import Types
import Vec2d


renderHUD :: LevelNumber -> Picture
renderHUD = translate (-315) 200
          . scale 0.2 0.2
          . blackText
          . printf "Level %d"


insideStage :: Stage -> Picture -> Picture
insideStage = insideGrid 60 60

renderStage :: Sprites -> Stage -> Picture
renderStage sprites = pictureGrid 60 60
                    . fmap (renderTile sprites)

atStagePos :: V Float -> Picture -> Picture
atStagePos = atGridPos 60 60


renderPlayer :: Sprites -> PlayerGraphics -> Picture
renderPlayer sprites (PlayerGraphics {..}) = guardPicture gPlayerVisible
                                           $ atStagePos gPlayerScreenPos
                                           $ renderPlayerSprite sprites


renderInventory :: Sprites -> Inventory -> Picture
renderInventory sprites = translate (-300) 180
                        . pictureRow 35
                        . fmap renderTinyKey
  where
    renderTinyKey :: KeyNumber -> Picture
    renderTinyKey k = scale 0.5 0.5
                    $ renderTile sprites (Key k)


renderDebugMessage :: String -> Picture
renderDebugMessage = translate (-315) (-235)
                   . scale 0.1 0.1
                   . blackText

renderDebugMessages :: [String] -> Picture
renderDebugMessages = pictureCol 11 . fmap renderDebugMessage


renderGameState :: Sprites -> GameState -> Picture
renderGameState sprites (GameState {..}) = renderHUD gLevelNumber
                                        <> renderStage sprites gStage
                                        <> insideStage gStage (renderPlayer sprites gPlayerGraphics)
                                        <> renderInventory sprites gInventory
                                        <> renderDebugMessages gDebugMessages
