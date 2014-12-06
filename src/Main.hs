module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Vec2d


mainBanana :: Frameworks t
           => Event t Float
           -> Event t InputEvent
           -> Moment t (Behavior t Picture)
mainBanana _ _ = return (pure (circle 10))

main :: IO ()
main = playBanana (InWindow "Ludum Dare 31" (640, 480) (800, 50))
                  white
                  60
                  mainBanana
