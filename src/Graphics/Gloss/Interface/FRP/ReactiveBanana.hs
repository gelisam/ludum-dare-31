{-# LANGUAGE UnicodeSyntax, Rank2Types #-}

module Graphics.Gloss.Interface.FRP.ReactiveBanana (playBanana, InputEvent) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A useful type synonym for Gloss event values, to avoid confusion between
--   Gloss and ReactiveBanana.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
--   Behavior t Picture changes.
playBanana ∷ Display -- ^ The display method
           → Color   -- ^ The background colour
           → Int     -- ^ The refresh rate, in Hertz
           → (∀ t. Frameworks t
              ⇒ Event t Float
              → Event t InputEvent
              → Moment t (Behavior t Picture))
           -- ^ A Moment t action to generate the Picture Behavior, taking
           --   the refresh and input Events with respect to which to build it.
           --   The refresh event generates a Float indicating the time delta
           --   since the last refresh.
           → IO ()
playBanana display colour frequency mPicture = do
  pictureref ← newIORef blank
  (tickHandler,  tick)  ← newAddHandler
  (eventHandler, event) ← newAddHandler
  compile (makeNetwork tickHandler eventHandler $ writeIORef pictureref) >>= actuate
  playIO display colour frequency ()
    (\      _ → readIORef pictureref)
    (\ ev   _ → () <$ event ev)
    (\ time _ → () <$ tick time)
  where
    makeNetwork tickHandler eventHandler change = do
      eTick  ← fromAddHandler tickHandler
      eEvent ← fromAddHandler eventHandler
      bRawPicture ← mPicture eTick eEvent
      
      -- make sure the Behavior doesn't leak memory if mPicture ignores
      -- one or both kind of events
      let bPicture = bRawPicture
                  <* stepper undefined eTick
                  <* stepper undefined eEvent
      
      changes bPicture >>= reactimate' . fmap (fmap change)
      initial bPicture >>= liftIO . change
