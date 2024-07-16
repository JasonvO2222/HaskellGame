module State where


import Graphics.Gloss
import Grid
import Moveable


data GameState = GameState {
      sprites :: [Picture],
      raster :: Raster,
      player :: Moveable,
      barriers :: [Tile],
      screenSize :: Float}

initState :: [Picture] -> Raster -> Moveable -> [Tile] -> Float -> GameState
initState s r m t sz = GameState {sprites = s, raster = r, player = m, barriers = t, screenSize = sz}