module State where



import Graphics.Gloss

data GameState = GameState {
      sprites :: [Picture]}

initState :: [Picture] -> GameState
initState p = GameState {sprites = p}