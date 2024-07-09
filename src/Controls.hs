module Controls where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (dupChan)
import System.IO
import Control.Exception (catch, IOException)

import State
import Moveable
import Grid


-- handle user input
input :: Event -> GameState -> IO (GameState)
input e gstate = do
    return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char c) Down _ _) gstate --if a key is pressed down, add it to list of pressed keys
   | c == 'w' = gstate { player = addDir p UpD}
   | c == 's' = gstate { player = addDir p DownD}
   | c == 'd' = gstate { player = addDir p RightD}
   | c == 'a' = gstate { player = addDir p LeftD}
   | c == 'b' = gstate { player = addDir p BrakeD}
   | otherwise = gstate
            where 
                p = player gstate
handleInput (EventKey (Char c) Up _ _) gstate --if a key is released up, remove from list of pressed keys
   | c == 'w' = gstate { player = removeDir p UpD}
   | c == 's' = gstate { player = removeDir p DownD}
   | c == 'd' = gstate { player = removeDir p RightD}
   | c == 'a' = gstate { player = removeDir p LeftD}
   | c == 'b' = gstate { player = removeDir p BrakeD}
   | otherwise = gstate
            where
                p = player gstate
handleInput _ gstate = gstate