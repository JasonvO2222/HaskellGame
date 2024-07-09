module Tick where
import State
import Moveable
import Grid


step :: Float -> GameState -> IO (GameState) -- runs every render frame
step seconds gstate = do
    updatedPlayer <- updatePlayer (raster gstate) (player gstate)
    return (gstate {player = updatedPlayer}) -- returns new gamestate after updating all parameters



updatePlayer :: Raster -> Moveable -> IO (Moveable)
updatePlayer r player = do
    updatedVector <- checkCollision r player (updateVector player)
    let updatedPoint = moveObject player updatedVector
    return (player {point = updatedPoint, vector = updatedVector})

checkCollision :: Raster -> Moveable -> Vector -> IO(Vector)
checkCollision r m v = do
    let vertical = checkVertical r (point m) (snd v)
    let horizontal = checkHorizontal r (point m) (fst v)
    return (correctVector vertical horizontal v)


checkVertical :: Raster -> Point -> Float -> Bool
checkVertical r p f | f == 0 = False
                    | f > 0 = Barrier == snd ((r!!ry)!!rx)
                    | f < 0 = Barrier == snd ((r!!ry)!!lx)
                        where
                            (x, y) = p
                            rx = round(x + 0.5)
                            lx = round(x - 0.5)
                            ry = round y

checkHorizontal :: Raster -> Point -> Float -> Bool
checkHorizontal r p f | f == 0 = False
                      | f > 0 = Barrier == snd ((r!!dy)!!rx)
                      | f < 0 = Barrier == snd ((r!!uy)!!rx)
                        where
                            (x, y) = p
                            uy = round(y - 0.5)
                            dy = round(y + 0.5)
                            rx = round x


correctVector :: Bool -> Bool -> Vector -> Vector
correctVector ver hor v | ver && hor = (0, 0)
                        | ver = (x, 0)
                        | hor = (0, y)
                        | otherwise = v
                        where
                            (x, y) = v