module Controls where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (dupChan)
import System.IO
import Control.Exception (catch, IOException)

import State
import Player
import Grid


-- handle user input
input :: Event -> GameState -> IO (GameState)
input e gstate = do
    return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char c) Down _ _) gstate
   | c == 'w' = gstate { player = addKey (p) (pressed p) UpK}
   | c == 's' = gstate { player = addKey (p) (pressed p) DownK}
   | c == 'd' = gstate { player = addKey (p) (pressed p) RightK}
   | c == 'a' = gstate { player = addKey (p) (pressed p) LeftK}
   | otherwise = gstate
            where 
                p = player gstate
handleInput (EventKey (Char c) Up _ _) gstate
   | c == 'w' = gstate { player = removeKey (p) (pressed p) UpK}
   | c == 's' = gstate { player = removeKey (p) (pressed p) DownK}
   | c == 'd' = gstate { player = removeKey (p) (pressed p) RightK}
   | c == 'a' = gstate { player = removeKey (p) (pressed p) LeftK}
   | otherwise = gstate
            where
                p = player gstate
handleInput _ gstate = gstate