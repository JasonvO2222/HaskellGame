module Grid where

data Dir = RightD | LeftD | UpD | DownD deriving (Show, Eq)

type Point = (Float, Float)

type Tile = (Point, Tiletype)

data Tiletype = Empty | Barrier  deriving (Show, Eq)

type Raster = [[Tile]] --list of lists, every list inside is a row of tiles

makeRaster :: [String] -> Raster
makeRaster file = map makeLine (zip [0..] file) -- zip with y-indexes

makeLine :: (Float, String) -> [Tile]
makeLine (f, s) = map (merge f) (map makeTile (zip [0..] s)) -- zip with x-indexes

makeTile :: (Float, Char) -> (Float, Tiletype) --add Tiletype data
makeTile (f, c) | c == '#' = (f, Barrier)
                | c == '-' = (f, Empty)

merge :: Float -> (Float, Tiletype) -> Tile -- reorder information
merge f (f2, t) = ((f, f2), t)

listBarriers :: Raster -> [Tile]
listBarriers [] = []
listBarriers (x:xs) = filter filt (x ++ listBarriers xs)

filt :: Tile -> Bool
filt t | (snd t) == Barrier = True
       | otherwise = False