module Game where

import Data.Ix
import GHC.Arr (Array, array)

data Player = PlayerX | PlayerO deriving (Eq, Show)

data Cell = Empty | Full Player deriving (Eq, Show)

data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game
  { gameBoard :: Board,
    gamePlayer :: Player,
    gameState :: State
  }

initialGame :: Game
initialGame =
  Game
    { gameBoard = array indexRange $ zip (range indexRange) (repeat Empty),
      gamePlayer = PlayerX,
      gameState = Running
    }
  where
    indexRange = ((0, 0), (2, 2))
