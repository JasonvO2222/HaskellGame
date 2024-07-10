module Tick where
import State
import Moveable
import Grid


step :: Float -> GameState -> IO (GameState) -- runs every render frame
step seconds gstate = do
    updatedPlayer <- updatePlayer gstate (raster gstate) (player gstate)
    return (gstate {player = updatedPlayer}) -- returns new gamestate after updating all parameters



updatePlayer :: GameState -> Raster -> Moveable -> IO (Moveable)
updatePlayer gstate r player = do

    --first check overlap with static types and other moveables for each step along the vector

    --find step vector based on set length
    let updatedVector = updateVector player
    let (x, y) = updatedVector
    let steps = findFactor (sqrt(x*x + y*y))
    let step = findStepVector steps (x, y)

    --calculate corrected vector and new point
    (correctVector, newPoint) <- calcVectorPoint gstate step steps




    return (player)

findStepVector :: Float -> Vector -> Vector
findStepVector steps (x, y) = (x * factor, y * factor)
                   where
                         factor = 1 / steps

findFactor :: Float -> Float
findFactor 0 = 1
findFactor f = iF(ceiling (f / 0.2))

calcVectorPoint :: GameState -> Vector -> Float -> IO (Vector, Point)
calcVectorPoint gstate step steps = do

    let (dx, dy) = step
    let (x, y) = point (player gstate)
    let l = [1 .. steps]
    let lm = map (\a -> (dx * a, dy * a)) l 
    let newPointList = map (\(b, c) -> (b + x, c + y)) lm


    (newStep, newPoint) <- iterateV (x, y) newPointList gstate step
    let newVector = (steps * (fst newStep), steps * (snd newStep))
    return (newVector, newPoint)

iterateV :: Point -> [(Float, Float)] -> GameState -> Vector -> IO (Vector, Point)
iterateV lastP [] _ step = do return (step, lastP)
iterateV lastP l@(x:xs) gstate step = do
    -- get corners
    let corners = getTLTRBLBR x 

    -- check if new point hits anything
    let hitCorners = checkCorners (raster gstate) (selectCorners step corners)

    -- if not go onto next point
    -- if it does, go back one point, construct new vector, calc new pointslist, go over them
    iterateHelper l (x, lastP) xs gstate step hitCorners

iterateHelper :: [(Float, Float)] -> (Point, Point) -> [(Float, Float)] -> GameState -> Vector -> [CornerT] -> IO (Vector, Point)
iterateHelper l (x, lastP) xs gstate step hitCorners | null hitCorners = iterateV x xs gstate step
                                                     | otherwise = iterateV lastP newxs gstate newStep
                                        where
                  (newxs, newStep) = handleHit lastP hitCorners step xs (iF (length l))



handleHit :: Point -> [CornerT] -> Vector -> [(Float, Float)] -> Float -> ([(Float, Float)], Vector)
handleHit lp [a, b, c] (x, y) xs l = (updateList lp newV l, newV)
                     where
                        newV = (-0.20 * x, -0.20 * y)
handleHit lp [a, b] (x, y) xs l | (a == TL && b == TR) || (a == BL && b == BR) = (updateList lp (x, y * (-0.20)) l, (x, y * (-0.20)))
                                | otherwise                                    = (updateList lp (x * (-0.20), y) l, (x * (-0.20), y))

updateList :: Point -> Vector -> Float -> [(Float, Float)]
updateList lp step count = newPointList
                      where
                         l = [1 .. count]
                         lm = map (\a -> ((fst step) * a, (snd step) * a)) l
                         newPointList = map (\(b, c) -> (b + (fst lp), c + (snd lp))) lm


checkCorners :: Raster -> [(Point, CornerT)] -> [CornerT]
checkCorners _ [] = []
checkCorners r ps = map snd (filter (\(p, c) -> checkRaster (round (fst p), round (snd p))) ps)
                where
                    checkRaster (column, row) = snd (r!!row!!column) == Barrier

selectCorners :: Vector -> ((Point, CornerT), (Point, CornerT), (Point, CornerT), (Point, CornerT)) -> [(Point, CornerT)]
selectCorners (x, y) (tl, tr, bl, br) | x > 0 && y > 0 = [tr, bl, br]
                                      | x < 0 && y < 0 = [tl, tr, bl]
                                      | x < 0 && y > 0 = [tl, bl, br]
                                      | x > 0 && y < 0 = [tl, tr, br]
                                      | x > 0 = [tr, br]
                                      | x < 0 = [tl, bl]
                                      | y > 0 = [bl, br]
                                      | y < 0 = [tl, tr]
                                      | otherwise = []


























checkCollision :: Raster -> Moveable -> Vector -> IO (Bool, Bool, Bool)
checkCollision r m v = do
    let vertical = checkVertical r (point m) (snd v)
    let horizontal = checkHorizontal r (point m) (fst v)
    let diagonal = checkDiagonal r (point m) v
    return (horizontal, vertical, diagonal)


checkHorizontal :: Raster -> Point -> Float -> Bool
checkHorizontal r p f | f == 0 = False
                      | f > 0 = Barrier == snd ((r!!ry)!!rx)
                      | f < 0 = Barrier == snd ((r!!ry)!!lx)
                        where
                            (x, y) = p
                            rx = round (x + 0.5)
                            lx = round (x - 0.5)
                            ry = round y

checkVertical :: Raster -> Point -> Float -> Bool
checkVertical r p f | f == 0 = False
                    | f > 0 = Barrier == snd ((r!!dy)!!rx)
                    | f < 0 = Barrier == snd ((r!!uy)!!rx)
                        where
                            (x, y) = p
                            uy = round (y - 0.5)
                            dy = round (y + 0.5)
                            rx = round x

checkDiagonal :: Raster -> Point -> Vector -> Bool
checkDiagonal r (x, y) v | x == 0 || y == 0 = False
                         | x > 0 && y > 0 = Barrier == snd ((r!!dy)!!rx)
                         | x < 0 && y > 0 = Barrier == snd ((r!!dy)!!lx)
                         | x > 0 && y < 0 = Barrier == snd ((r!!uy)!!rx)
                         | x < 0 && y < 0 = Barrier == snd ((r!!uy)!!lx)
                            where
                            uy = round (y - 0.5)
                            dy = round (y + 0.5)
                            rx = round (x + 0.5)
                            lx = round (x - 0.5)

correctVector :: GameState -> (Bool, Bool, Bool) -> Vector -> Vector
correctVector gstate (hor, ver, dia) (x, y) | ver && hor = (0, 0)
                                            | ver = (x, 0)
                                            | hor = (0, y)
                                            | dia = correctVectorDiagonal gstate (x, y)
                                            | otherwise = (x, y)


correctPoint :: GameState -> (Bool, Bool, Bool) -> Point -> Point
correctPoint gstate (hor, ver, dia) (x, y) | hor && ver = ( iF (round x), iF (round y))
                                           | hor = (iF (round x), y)
                                           | ver = (x, iF (round y))
                                           | dia = correctPointDiagonal gstate (x, y)
                                           | otherwise = (x, y)


correctVectorDiagonal :: GameState -> Vector -> Vector
correctVectorDiagonal gstate (x, y) | abs (x - iF (round x)) < abs (y - iF (round y)) = (0, y)
                                    | otherwise = (x, 0)


correctPointDiagonal :: GameState -> Point -> Point
correctPointDiagonal gstate (x, y) | abs (x - iF (round x)) < abs (y - iF (round y)) = (iF (round x), y)
                                   | otherwise = (x, iF (round y))

iF :: Int -> Float
iF i = fromIntegral i :: Float



