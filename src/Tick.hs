module Tick where

import State








--step func
step :: Float -> GameState -> IO GameState
step seconds gstate = return $ gstate