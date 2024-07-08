module State where


import Graphics.Gloss
import Grid
import Player

--data PictureL = PictureL { name :: String, bmp :: Picture} deriving (Show, Eq)


data GameState = GameState {
      sprites :: [Picture],
      raster :: Raster,
      player :: Player,
      barriers :: [Tile]}

initState :: [Picture] -> Raster -> Player -> [Tile] -> GameState
initState s r p t = GameState {sprites = s, raster = r, player = p, barriers = t}