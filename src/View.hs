module View where

import Graphics.Gloss

import State
import Grid
import Moveable
import Tick


translateP :: Float -> Picture -> Grid.Point -> (Float, Float) -> Float -> Picture --translate picture to its screen location
translateP blockSize p (x, y) (sx, sy) offset = translate (x*blockSize - offset + 0.5 * blockSize) (offset - y*blockSize - 0.5 * blockSize) (scale sx sy p)

--viewing method
view :: GameState -> IO (Picture)
view gstate = do
   
   let size = screenSize gstate -- size of the screen
   let blockSize =  size / (iF (length (raster gstate))) -- size of one block in the raster               replace with current raster (allows for different sizes of rooms), and 400 for game window size
   let offset = size / 2 -- get topleft pixelpoint
   let barrierScale = blockSize / (iF 22)  -- insert here the pixel size of the sprites we are useing instead of 22!!!!
   let backgroundScale = blockSize / (iF 64) --
   let playerScale = blockSize / (iF 32)

   let sprite = sprites gstate
   let background = generateBackGround blockSize barrierScale backgroundScale (raster gstate) sprite offset

   let getPlayer = viewPlayer blockSize playerScale ((sprite)!!1) (player gstate) offset
   --let onF = viewoFB blockSize sizefac (corners (player gstate)) ((sprite)!!0) (player gstate)
   --let oCS = viewoCS blockSize sizefac (corners (player gstate)) ((sprite)!!0) (player gstate)
   

   return $ pictures (background ++ [getPlayer]) -- returns list of pictures


viewPlayer :: Float -> Float -> Picture -> Moveable -> Float -> Picture
viewPlayer blockSize scale p player offset = translateP blockSize p (point player) (scale, scale) offset


generateBackGround :: Float -> Float -> Float -> Raster -> [Picture] -> Float -> [Picture]
generateBackGround blockSize scaleB scaleBG [] ps offset = []
generateBackGround blockSize scaleB scaleBG (x:xs) ps offset = generateRow blockSize scaleB scaleBG x ps offset ++ generateBackGround blockSize scaleB scaleBG xs ps offset

generateRow :: Float -> Float -> Float -> [Tile] -> [Picture] -> Float -> [Picture]
generateRow blockSize scaleB scaleBG [] p offset = []
generateRow blockSize scaleB scaleBG (t:ts) p offset | snd t == Barrier = translateP blockSize (p!!0) (fst t) (scaleB, scaleB) offset  : generateRow blockSize scaleB scaleBG ts p offset
                                                     | snd t == Empty   = translateP blockSize (p!!2) (fst t) (scaleBG, scaleBG) offset : generateRow blockSize scaleB scaleBG ts p offset






--for testing
--viewoFB :: Float -> Float -> Int -> Picture -> Moveable -> [Picture]
--viewoFB fac scale 0 p m = []
--viewoFB fac scale c p m | (onFloor m) = (translateP fac p (fromIntegral c :: Float, 1) (scale, scale)) : (viewoFB fac scale (c-1) p m)
--                        | otherwise = []

--viewoCS :: Float -> Float -> Int -> Picture -> Moveable -> [Picture]
--viewoCS fac scale  0 p m = []
--viewoCS fac scale c p m = (translateP fac p (fromIntegral c :: Float, 2) (scale, scale)) : (viewoFB fac scale (c-1) p m)