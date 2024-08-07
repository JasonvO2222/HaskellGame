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
   background <- loadBMP "src/sprites/Green.bmp"
   let pictures = [block, playerFrog, background]

   cage <- readFile "src/levels/cage.txt"
   let screenSize = 1000 :: Int
   let raster = Grid.makeRaster (lines cage)
   let rl = iF (length raster)
   let player = makeMoveable (rl / 2 , rl / 2) 0.05 0.3 Player 0.15 0.40

   let state = State.initState pictures raster player (listBarriers raster) (iF screenSize)


   playIO (InWindow "Game" (screenSize, screenSize) (0, 0))
          black --bg color
          144 --fps
          state --state datastructure with all info
          view --state to IO picture method for screen
          input --method that handles user input
          step --step function
        

