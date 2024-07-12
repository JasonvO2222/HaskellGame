module Moveable where

import Grid

data ObjType = Player | Other deriving (Show, Eq)

data CornerT = TL | TR | BL | BR deriving (Show, Eq)

data Moveable = Moveable { point :: Point,
                           accX :: Float,
                           accY :: Float,
                           dirs :: [Dir],
                           objtype :: ObjType,
                           vector :: Vector,
                           maxSpeedX :: Float,
                           maxSpeedY :: Float,
                           onFloor :: Bool,
                           corners :: Int -- for testing purposes
                           }

makeMoveable :: Point -> Float -> Float -> ObjType -> Float -> Float -> Moveable
makeMoveable p accx accy t maxX maxY = Moveable {point = p, accX = accx, accY = accy, objtype = t, dirs = [], vector = (0, 0), maxSpeedX = maxX, maxSpeedY = maxY, onFloor = False, corners = 0} 


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
                mSpeed = (maxSpeedX m, maxSpeedY m)
                acc = (accX m, accY m)
                vec = vector m

iterateDirs :: Bool -> (Float, Float) -> Vector -> [Dir] -> Vector -- iterate move over each Dir and fold results
--iterateDirs b a v ks = foldl (newVec a) v ks -- for testing remove later
iterateDirs b a v ks | b = foldl (newVec a) v ks
                     | otherwise = v

newVec :: (Float, Float) -> Vector -> Dir -> Vector -- move in direction
newVec (accx, accy) v k | k == UpD = (x, y - accy)
                        | k == DownD = (x, y + accy)
                        | k == RightD = (x + accx, y)
                        | k == LeftD = (x - accx, y)
                        | k == BrakeD = (brake accx x, brake accy y)
                        | otherwise = (x, y)
    where
        (x, y) = v

brake :: Float -> Float -> Float
brake a f | f > a = f - a
          | f < -(a) = f + a
          | otherwise = 0

checkMaxSpeed :: (Float, Float) -> Vector -> Vector
checkMaxSpeed (maxX, maxY) (x, y) = (checkSpeed maxX x, checkSpeed maxY y)

checkSpeed :: Float -> Float -> Float
checkSpeed max f | f > max = max
                 | f < -(max) = -(max)
                 | otherwise = f

addGravity :: Bool -> Vector -> Vector
addGravity b (x, y) | not b = (x, y + 0.040)
                    | otherwise = (x, y)

-- not used, might delete later
moveObject :: Point -> Vector -> Point
moveObject (x, y) (a, b) = (x + a, y + b)

-- TL: topLeft, TR: topRight.....
getTLTRBLBR :: Point -> [(Point, CornerT)]
getTLTRBLBR (x, y) = [((x - 0.5, y - 0.5), TL), ((x + 0.5, y - 0.5), TR), ((x - 0.5, y + 0.55), BL), ((x + 0.5, y + 0.55), BR)]

