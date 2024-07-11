module Tick where
import State
import Moveable
import Grid


step :: Float -> GameState -> IO (GameState) -- runs every render frame
step seconds gstate = do
    updatedPlayer <- updatePlayer gstate (raster gstate) (player gstate)
    return (gstate {player = updatedPlayer}) -- returns new gamestate after updating all parameters


-- In each tick the movement from the current point to the next point defined by the movement vector, 
--      is split into steps no larger than a set value. This avoids the moveable object from going through walls at high speeds.
-- A list of points is created with the location of where the object will be at each step.
-- The code iterates through each point, calculating the corners of the object at that point, and looking if it intersects with a barrier.
-- If it doesnt intersect it just moves on to the next point until it arrives at the end of the list and returns the new point of the object.
-- If there is an intersection, we determine what kind of intersection (wall on the left, floor, etc) it is by looking at which corners intersect.
-- For each kind of intersect, the vector is changed differently according to logic (lmao). 
-- Then it looks at how many steps are still in the current list, 
--     and creates a new list of the remaining points however now calculated using the new vector, from the last point before the intersect

updatePlayer :: GameState -> Raster -> Moveable -> IO (Moveable)
updatePlayer gstate r player = do

    -- IGNORE!! (not implemented yet) first check overlap with static types and other moveables for each step along the vector
    let updatedVector = updateVector player
    let (x, y) = updatedVector

    --find step vector based on set length
    let steps = findFactor (sqrt(x*x + y*y))
    let step = findStepVector steps (x, y)

    --calculate corrected vector and new point
    (correctVector, newPoint) <- calcVectorPoint gstate step steps

    return (player {vector = correctVector, point = newPoint})

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
    -- create list of point where the object will move
    let l = [1 .. steps]
    let lm = map (\a -> (dx * a, dy * a)) l 
    let newPointList = map (\(b, c) -> (b + x, c + y)) lm

    -- iterate through points
    (newStep, newPoint) <- iterateV (x, y) newPointList gstate step
    
    -- remove the factor (multiply by amount of steps the vector was split by) so we get the full movement vector again
    let newVector = (steps * (fst newStep), steps * (snd newStep))
    return (newVector, newPoint)

iterateV :: Point -> [(Float, Float)] -> GameState -> Vector -> IO (Vector, Point)
iterateV lastP [] _ step = do return (step, lastP)
iterateV lastP l@(x:xs) gstate step = do

    -- check if corners hit anything
    let corners = getTLTRBLBR x 
    let hitCorners = checkCorners (raster gstate) (selectCorners step corners)

    -- if not go onto next point
    -- if it does, go back one point, construct new vector, calc new pointslist, go over them
    iterateHelper l (x, lastP) xs gstate step hitCorners

iterateHelper :: [(Float, Float)] -> (Point, Point) -> [(Float, Float)] -> GameState -> Vector -> [CornerT] -> IO (Vector, Point)
iterateHelper l (x, lastP) xs gstate step hitCorners | null hitCorners = iterateV x xs gstate step -- if the corners dont hit anything move on to next point
                                                     | otherwise = iterateV lastP newxs gstate newStep -- use new list and vector and move on
                                        where
                  (newxs, newStep) = handleHit lastP hitCorners step xs (iF (length l)) -- handleHit calculates the new vec and pointList


-- we can differentiate between what kind of collision we have by the amount of corners are hit (hit 3, 2, or 1 corners) (see [a,b,c], [a,b] and [a])
handleHit :: Point -> [CornerT] -> Vector -> [(Float, Float)] -> Float -> ([(Float, Float)], Vector)
handleHit lp [a, b, c] (x, y) xs l = (updateList lp newV l, newV) -- hit a corner of barriers (bounce back on all axes)
                     where
                        newV = (-0.20 * x, -0.20 * y)
handleHit lp [a, b] (x, y) xs l | (a == TL && b == TR) || (a == BL && b == BR) = (updateList lp (x, y * (-0.20)) l, (x, y * (-0.20))) -- one case for collision in y direction
                                | otherwise                                    = (updateList lp (x * (-0.20), y) l, (x * (-0.20), y)) -- and for x direction
handleHit lp [a] (x, y) xs l | x /= 0 && y /= 0 = (updateList lp (greatestStays (x, y)) l, greatestStays (x, y)) -- hit a single barrier from an angle, the direction with the most speed stays (maybe not a good idea)
                             | x /= 0 = (updateList lp (x * (-0.20), y) l, (x * (-0.20), y)) -- unless ofc the object is moving in only on direction
                             | y /= 0 = (updateList lp (x, (-0.20) *y) l, (x, (-0.20) * y))

greatestStays :: Vector -> Vector
greatestStays (x, y) | abs x > abs y = (x, y * (-0.20))
                     | otherwise = (x * (-0.20), y)

-- helps creating a new list of points
updateList :: Point -> Vector -> Float -> [(Float, Float)]
updateList lp step count = newPointList
                      where
                         l = [1 .. count]
                         lm = map (\a -> ((fst step) * a, (snd step) * a)) l
                         newPointList = map (\(b, c) -> (b + (fst lp), c + (snd lp))) lm

-- corner stuff
checkCorners :: Raster -> [(Point, CornerT)] -> [CornerT]
checkCorners _ [] = []
checkCorners r ps = map snd (filter (\(p, c) -> checkRaster (round (fst p), round (snd p))) ps)
                where
                    checkRaster (column, row) = isB (r!!row!!column)

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


-- VERY USEFUL int to float method
iF :: Int -> Float
iF i = fromIntegral i :: Float



