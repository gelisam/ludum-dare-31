module Input where

import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Vec2d


-- WASD, arrow keys, or HJKL
key2dir :: Num a => Key -> Maybe (V a)
key2dir (Char 'w') = Just up
key2dir (Char 'a') = Just left
key2dir (Char 's') = Just down
key2dir (Char 'd') = Just right
key2dir (SpecialKey KeyUp)    = Just up
key2dir (SpecialKey KeyDown)  = Just down
key2dir (SpecialKey KeyLeft)  = Just left
key2dir (SpecialKey KeyRight) = Just right
key2dir (Char 'h') = Just left
key2dir (Char 'j') = Just down
key2dir (Char 'k') = Just up
key2dir (Char 'l') = Just right
key2dir _ = Nothing

keydown2dir :: Num a => InputEvent -> Maybe (V a)
keydown2dir (EventKey k Down _ _) = key2dir k
keydown2dir _ = Nothing
