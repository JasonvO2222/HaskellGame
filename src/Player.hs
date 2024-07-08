module Player where

import Grid

data Player = Player { point :: Grid.Point,
                       direction :: Direction,
                       speed :: Float}


movePlayer :: Player -> Point
movePlayer player  | d == UpD = (x, y + s)
                   | d == DownD = (x, y - s)
                   | d == RightD = (x + s, y)
                   | d == LeftD = (x - s, y)
                   | otherwise = (x, y)
    where
        (x, y) = point player
        d = direction player
        s = speed player

changePlayerDir :: Player -> Direction -> Player
changePlayerDir p d = p {direction = d}