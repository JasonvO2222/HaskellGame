module Moveable where

import Grid

data ObjType = Player | Other deriving (Show, Eq)

data Moveable = Moveable { point :: Point,
                           speed :: Float,
                           dirs :: [Dir],
                           objtype :: ObjType}

addDir :: Moveable -> Dir -> Moveable
addDir m d = m {dirs = (d : dirs m)}

removeDir :: Moveable -> Dir -> Moveable
removeDir m d = m {dirs = filt}
          where
             ds = dirs m
             filt = filter (/= d) ds

moveObject :: Moveable -> Point -- extract information
moveObject m = iterateDirs (speed m) (point m) (dirs m)

iterateDirs :: Float -> Point -> [Dir] -> Point -- iterate move over each Dir and fold results
iterateDirs s p ks = foldl (move s) p ks

move :: Float -> Point -> Dir -> Point -- move in direction
move s p k | k == UpD = (x, y + s)
           | k == DownD = (x, y - s)
           | k == RightD = (x + s, y)
           | k == LeftD = (x - s, y)
           | otherwise = (x, y)
    where
        (x, y) = p


makeMoveable :: Point -> Float -> ObjType -> Moveable
makeMoveable p s t = Moveable {point = p, speed = s, objtype = t, dirs = []} 