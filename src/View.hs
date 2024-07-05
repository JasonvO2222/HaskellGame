module View where

import Graphics.Gloss

import State

--viewing method
view :: GameState -> IO Picture
view gstate = do
   let background = sprites gstate!!0
   let res = [background]
   return $ pictures (res)