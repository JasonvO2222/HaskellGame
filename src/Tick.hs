module Tick where

import State
import Moveable



step :: Float -> GameState -> IO (GameState) -- runs every render frame
step seconds gstate = do
    updatedPlayer <- updatePlayer (player gstate)
    return (gstate {player = updatedPlayer}) -- returns new gamestate after updating all parameters



updatePlayer :: Moveable -> IO (Moveable)
updatePlayer player = do
    let updatedPoint = moveObject player 
    return (player {point = updatedPoint})