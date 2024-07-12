module View where

import Graphics.Gloss

import State
import Grid
import Moveable
import Tick


translateP :: Float -> Picture -> Grid.Point -> (Float, Float) -> Picture --translate picture to its screen location
translateP fac p (x, y) (sx, sy) = translate (x*fac - 200 + 0.5 * fac) (200 - y*fac - 0.5 * fac) (scale sx sy p)

--viewing method
view :: GameState -> IO (Picture)
view gstate = do
   
   let fac =  (iF 400) / (iF (length (raster gstate))) -- replace with current raster (allows for different sizes of rooms), and 400 for game window size
   let sizefac = fac / (iF 22)  -- insert here the pixel size of the sprites we are useing instead of 22!!!!
   let sfbg = fac / (iF 64)
   let playerScale = fac / (iF 32)

   let sprite = sprites gstate
   let background = generateBackGround fac sizefac sfbg (raster gstate) sprite

   let getPlayer = viewPlayer fac playerScale ((sprite)!!1) (player gstate)
   let onF = viewoFB fac sizefac (corners (player gstate)) ((sprite)!!0) (player gstate)
   let oCS = viewoCS fac sizefac (corners (player gstate)) ((sprite)!!0) (player gstate)
   

   return $ pictures (background ++ oCS ++ onF ++ [getPlayer]) -- returns list of pictures

viewoFB :: Float -> Float -> Int -> Picture -> Moveable -> [Picture]
viewoFB fac scale 0 p m = []
viewoFB fac scale c p m | (onFloor m) = (translateP fac p (fromIntegral c :: Float, 1) (scale, scale)) : (viewoFB fac scale (c-1) p m)
                        | otherwise = []

viewoCS :: Float -> Float -> Int -> Picture -> Moveable -> [Picture]
viewoCS fac scale  0 p m = []
viewoCS fac scale c p m = (translateP fac p (fromIntegral c :: Float, 2) (scale, scale)) : (viewoFB fac scale (c-1) p m)


viewPlayer :: Float -> Float -> Picture -> Moveable -> Picture
viewPlayer fac scale p player = translateP fac p (point player) (scale, scale)


generateBackGround :: Float -> Float -> Float -> Raster -> [Picture] -> [Picture]
generateBackGround fac scaleB scaleBG [] ps = []
generateBackGround fac scaleB scaleBG (x:xs) ps = generateRow fac scaleB scaleBG x ps ++ generateBackGround fac scaleB scaleBG xs ps

generateRow :: Float -> Float -> Float -> [Tile] -> [Picture] -> [Picture]
generateRow fac scaleB scaleBG [] p = []
generateRow fac scaleB scaleBG (t:ts) p | snd t == Barrier = translateP fac (p!!0) (fst t) (scaleB, scaleB) : generateRow fac scaleB scaleBG ts p
                                        | snd t == Empty = translateP fac (p!!2) (fst t) (scaleBG, scaleBG) : generateRow fac scaleB scaleBG ts p
