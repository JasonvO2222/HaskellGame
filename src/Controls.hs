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
   | c == 'w' = gstate { player = changePlayerDir (player gstate) UpD}
   | c == 's' = gstate { player = changePlayerDir (player gstate) DownD}
   | c == 'd' = gstate { player = changePlayerDir (player gstate) RightD}
   | c == 'a' = gstate { player = changePlayerDir (player gstate) LeftD}
   | c == ' ' = gstate { player = changePlayerDir (player gstate) NeutralD}
   | otherwise = gstate
            where 
                p =  point (player gstate)
handleInput _ gstate = gstate