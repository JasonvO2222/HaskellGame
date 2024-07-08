module State where


import Graphics.Gloss
import Grid
import Moveable


data GameState = GameState {
      sprites :: [Picture],
      raster :: Raster,
      player :: Moveable,
      barriers :: [Tile]}

initState :: [Picture] -> Raster -> Moveable -> [Tile] -> GameState
initState s r m t = GameState {sprites = s, raster = r, player = m, barriers = t}