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
          . uscale 0.2
          . blackText
          . printf "Level %d"


insideStage :: Stage -> Picture -> Picture
insideStage = insideGrid 60 60

renderStage :: Sprites -> Stage -> Picture
renderStage sprites = pictureGrid 60 60
                    . fmap (renderTile sprites)

renderStageTop :: Sprites -> Stage -> Picture
renderStageTop sprites = pictureGrid 60 60
                       . fmap (renderTileTop sprites)

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
    renderTinyKey k = uscale 0.5
                    $ renderInventoryKey sprites k


renderDebugMessage :: String -> Picture
renderDebugMessage = translate (-315) (-235)
                   . uscale 0.1
                   . blackText

renderDebugMessages :: [String] -> Picture
renderDebugMessages = pictureCol 11 . fmap renderDebugMessage


renderGameState :: Sprites -> GameState -> Picture
renderGameState sprites (GameState {..}) = renderHUD gLevelNumber
                                        <> renderStage sprites gStage
                                        <> insideStage gStage (renderPlayer sprites gPlayerGraphics)
                                        <> renderStageTop sprites gStage
                                        <> renderInventory sprites gInventory
                                        <> renderDebugMessages gDebugMessages
