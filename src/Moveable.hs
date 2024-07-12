module Moveable where

import Grid

data ObjType = Player | Other deriving (Show, Eq)

data CornerT = TL | TR | BL | BR deriving (Show, Eq)

data Moveable = Moveable { point :: Point,
                           acceleration :: Float,
                           dirs :: [Dir],
                           objtype :: ObjType,
                           vector :: Vector,
                           maxSpeed :: Float,
                           onFloor :: Bool}

makeMoveable :: Point -> Float -> ObjType -> Float -> Moveable
makeMoveable p a t max = Moveable {point = p, acceleration = a, objtype = t, dirs = [], vector = (0, 0), maxSpeed = max, onFloor = False} 


addDir :: Moveable -> Dir -> Moveable
addDir m d = m {dirs = (d : dirs m)}

removeDir :: Moveable -> Dir -> Moveable
removeDir m d = m {dirs = filt}
          where
             ds = dirs m
             filt = filter (/= d) ds


updateVector :: Moveable -> Vector -- bring together all other methods (checkSpeed, Gravity, Controls) & extract information from object
updateVector m = checkMaxSpeed mSpeed (addGravity onfloor (iterateDirs onfloor acc vec (dirs m)))
             where
                onfloor = onFloor m
                mSpeed = maxSpeed m
                acc = acceleration m
                vec = vector m

iterateDirs :: Bool -> Float -> Vector -> [Dir] -> Vector -- iterate move over each Dir and fold results
iterateDirs b a v ks = foldl (newVec a) v ks -- for testing remove later
iterateDirs b a v ks | b = foldl (newVec a) v ks
                     | otherwise = v

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

addGravity :: Bool -> Vector -> Vector
--addGravity b (x, y) = (x, y + 0.2) -- for testing, remove later
addGravity b (x, y) | not b = (x, y + 0.2)
                    | otherwise = (x, y)

-- not used, might delete later
moveObject :: Point -> Vector -> Point
moveObject (x, y) (a, b) = (x + a, y + b)

-- TL: topLeft, TR: topRight.....
getTLTRBLBR :: Point -> [(Point, CornerT)]
getTLTRBLBR (x, y) = [((x - 0.5, y - 0.5), TL), ((x + 0.5, y - 0.5), TR), ((x - 0.5, y + 0.5), BL), ((x + 0.5, y + 0.5), BR)]

