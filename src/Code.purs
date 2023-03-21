module Code
  ( privDir
  ) where

import Data.Maybe (Maybe)
import Effect (Effect)

foreign import privDir :: String -> Effect (Maybe String)
