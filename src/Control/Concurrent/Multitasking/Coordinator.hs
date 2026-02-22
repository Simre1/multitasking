module Control.Concurrent.Multitasking.Coordinator where

import GHC.Generics
import Ki.Unlifted qualified as Ki

data Coordinator = Coordinator
  { scope :: Ki.Scope,
    options :: Ki.ThreadOptions
  }
  deriving (Generic)

modifyThreadOptions :: (Ki.ThreadOptions -> Ki.ThreadOptions) -> Coordinator -> Coordinator
modifyThreadOptions f c@(Coordinator {options}) = c {options = f options}
