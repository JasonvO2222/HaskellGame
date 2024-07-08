module Main where

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO)

import Graphics.Gloss

--import used modules here!
import View
import State
import Controls
import Tick
import Grid
import Moveable

main :: IO ()
main = do

   block <- loadBMP "src/sprites/Idle.bmp"
   playerFrog <- loadBMP "src/sprites/Jump (32x32).bmp"
   let pictures = [block, playerFrog]

   cage <- readFile "src/levels/cage.txt"
   let raster = Grid.makeRaster (lines cage)
   let player = makeMovable (10, 10) 0.2 Player

   let state = State.initState pictures raster player (listBarriers raster)


   playIO (InWindow "Game" (400, 400) (0, 0))
          black --bg color
          20 --fps
          state --state datastructure with all info
          view --state to IO picture method for screen
          input --method that handles user input
          step --step function
        

