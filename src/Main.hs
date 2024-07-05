module Main where

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(InWindow), playIO)

import Graphics.Gloss

--import used modules here!
import View
import State
import Controls
import Tick


main :: IO ()
main = do

   bg <- loadBMP "src/sprites/bg.jpg"
   let pictures = [bg]
   let state = State.initState pictures
    

   playIO (InWindow "Game" (400, 400) (0, 0))
      black --bg color
      24 --fps
      state --state datastructure with all info
      view --state to IO picture method for screen
      input --method that handles user input
      step --step function
        

