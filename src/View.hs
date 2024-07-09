module View where

import Graphics.Gloss

import State
import Grid
import Moveable


translateP :: Picture -> Grid.Point -> (Float, Float) -> Picture --translate picture to its screen location
translateP p (x, y) (sx, sy) = translate (x*20 - 200 + 0.5 * sx) (200 - y*20 - 0.5 * sy) p

--viewing method
view :: GameState -> IO (Picture)
view gstate = do
   
   let getEnvironment = map (viewEnvironment (head (sprites gstate))) (barriers gstate)
   let getPlayer = viewPlayer ((sprites gstate)!!1) (player gstate)

   return $ pictures (getEnvironment ++ [getPlayer]) -- returns list of pictures

viewPlayer :: Picture -> Moveable -> Picture
viewPlayer p player = translateP p (point player) (32, 32)


viewEnvironment :: Picture -> Tile -> Picture
viewEnvironment p t = translateP p (fst t) (20, 20)
