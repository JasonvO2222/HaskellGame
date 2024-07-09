module Moveable where

import Grid

data ObjType = Player | Other deriving (Show, Eq)

data Moveable = Moveable { point :: Point,
                           acceleration :: Float,
                           dirs :: [Dir],
                           objtype :: ObjType,
                           vector :: Vector,
                           maxSpeed :: Float}

makeMoveable :: Point -> Float -> ObjType -> Float -> Moveable
makeMoveable p a t max = Moveable {point = p, acceleration = a, objtype = t, dirs = [], vector = (0, 0), maxSpeed = max} 


addDir :: Moveable -> Dir -> Moveable
addDir m d = m {dirs = (d : dirs m)}

removeDir :: Moveable -> Dir -> Moveable
removeDir m d = m {dirs = filt}
          where
             ds = dirs m
             filt = filter (/= d) ds


updateVector :: Moveable -> Vector -- extract information
updateVector m = checkMaxSpeed (maxSpeed m) (iterateDirs (acceleration m) (vector m) (dirs m))

iterateDirs :: Float -> Vector -> [Dir] -> Vector -- iterate move over each Dir and fold results
iterateDirs a p ks = foldl (newVec a) p ks

newVec :: Float -> Vector -> Dir -> Vector -- move in direction
newVec a v k | k == UpD = (x, y - a)
             | k == DownD = (x, y + a)
             | k == RightD = (x + a, y)
             | k == LeftD = (x - a, y)
             | k == BrakeD = (brake a x, brake a y)
             | otherwise = (x, y)
    where
        (x, y) = v

brake :: Float -> Float -> Float
brake a f | f > a = f - a
          | f < -(a) = f + a
          | otherwise = 0

checkMaxSpeed :: Float -> Vector -> Vector
checkMaxSpeed max (x, y) = (checkSpeed max x, checkSpeed max y)

checkSpeed :: Float -> Float -> Float
checkSpeed max f | f > max = max
                 | f < -(max) = -(max)
                 | otherwise = f



moveObject :: Moveable -> Vector -> Point
moveObject m v = (x + a, y + b)
         where
            (x, y) = point m
            (a, b) = v