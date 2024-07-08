module Player where

import Grid

data Player = Player { point :: Grid.Point,
                       direction :: Direction,
                       speed :: Float,
                       pressed :: [Key]}

data Key = UpK | DownK | LeftK | RightK deriving (Show, Eq)


addKey :: Player -> [Key] -> Key -> Player
addKey p ks k = p {pressed = (k : ks)}


removeKey :: Player -> [Key] -> Key -> Player
removeKey p ks k = p {pressed = filt}
          where
             filt = filter (/= k) ks


movePlayer :: Player -> Point -- extract information
movePlayer p = iterateKeys (speed p) (point p) (pressed p)

iterateKeys :: Float -> Point -> [Key] -> Point -- iterate over each key and fold results
iterateKeys s p ks = foldl (move s) p ks

move :: Float -> Point -> Key -> Point -- move character in direction of key
move s p k | k == UpK = (x, y + s)
           | k == DownK = (x, y - s)
           | k == RightK = (x + s, y)
           | k == LeftK = (x - s, y)
           | otherwise = (x, y)
    where
        (x, y) = p



changePlayerDir :: Player -> Direction -> Player
changePlayerDir p d = p {direction = d}