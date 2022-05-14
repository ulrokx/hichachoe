module Lib
  ( playGame,
  )
where

import Game
import Graphics.Gloss
import Logic
import Rendering
import Constants

playGame :: IO ()
playGame = play window white fps initialGame renderGame handleEvents (const id)
