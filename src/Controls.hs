module Controls where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (dupChan)
import System.IO
import Control.Exception (catch, IOException)

import State


-- handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return $ gstate