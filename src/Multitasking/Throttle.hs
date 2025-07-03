module Multitasking.Throttle
  ( -- ** Throttling
    Throttle,
  )
where

import Multitasking.Core

data Throttle = MaxActiveThreads Int |  

throttle :: Throttle -> IO a -> IO a
throttle = undefined
