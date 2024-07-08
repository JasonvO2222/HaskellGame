module Tick where

import State
import Player



--step func
step :: Float -> GameState -> IO (GameState)
step seconds gstate = do
    updatedPlayer <- updatePlayer (player gstate)
    return (gstate {player = updatedPlayer})



updatePlayer :: Player -> IO (Player)
updatePlayer player = do
    let updatedPoint = movePlayer player 
    return (player {point = updatedPoint})