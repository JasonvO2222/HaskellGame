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
    let updatedVector = updateVector player
    collide <- checkCollision r player updatedVector
    let correctedVector = correctVector gstate collide updatedVector

    let correctedPoint = correctPoint gstate collide (point player)
    let updatedPoint = moveObject correctedPoint correctedVector

    return (player {point = updatedPoint, vector = correctedVector})

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



