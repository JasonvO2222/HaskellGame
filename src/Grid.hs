module Grid where

data Direction = RightD | LeftD | UpD | DownD | NeutralD deriving (Show, Eq)

type Point = (Float, Float)

type Tile = (Point, Tiletype)

data Tiletype = Empty | Barrier  deriving (Show, Eq)

type Raster = [[Tile]]

makeRaster :: [String] -> Raster
makeRaster file = map makeLine (zip [0..] file) -- do makeline for every stringline matched with their y-val

makeLine :: (Float, String) -> [Tile]
makeLine (f, s) = map (merge f) (map makeTile (zip [0..] s))

makeTile :: (Float, Char) -> (Float, Tiletype)
makeTile (f, c) | c == '#' = (f, Barrier)
                | c == '-' = (f, Empty)

merge :: Float -> (Float, Tiletype) -> Tile
merge f (f2, t) = ((f, f2), t)

listBarriers :: Raster -> [Tile]
listBarriers [] = []
listBarriers (x:xs) = filter filt (x ++ listBarriers xs)

filt :: Tile -> Bool
filt t | (snd t) == Barrier = True
       | otherwise = False