{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}
module InputBlocking where

import Control.Applicative
import Reactive.Banana


data InputBlocking t a = InputBlocking
  { inputBlockingValue :: Behavior t a
  , isBlockingInput :: Behavior t Bool
  , inputUnblocked :: Event t ()
  } deriving Functor

instance Applicative (InputBlocking t) where
    pure x = InputBlocking (pure x) (pure False) never
    af <*> ax = InputBlocking inputBlockingValue'
                              isBlockingInput'
                              inputUnblocked'
      where
        inputBlockingValue' = inputBlockingValue af <*> inputBlockingValue ax
        isBlockingInput' = (||) <$> isBlockingInput af <*> isBlockingInput ax
        inputUnblocked' = whenE (not <$> isBlockingInput')
                              $ inputUnblocked af
                        `union` inputUnblocked ax
