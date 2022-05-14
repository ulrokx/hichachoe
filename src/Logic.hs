module Logic where

import Data.List
import Data.Maybe
import GHC.Arr
import Game
import Graphics.Gloss.Interface.IO.Game
import Rendering

-- handleEvents e world = world

handleEvents :: Event -> Game -> Game
handleEvents e world
  | EventKey (MouseButton LeftButton) Down _ (mx, my) <- e,
    Game {} <- world =
    handleClick mx my world
  | otherwise = world

handleClick :: Float -> Float -> Game -> Game
handleClick mx my world
  | Game _ _ (GameOver _) <- world = initialGame
  | Game board _ Running <- world = if ifPlayed board i then world else checks newWorld
  where
    newWorld = gameAfterTurn world i
    i = getRCofClick mx my
    ifPlayed b i = case b ! i of
      Full _ -> True
      _ -> False

checks :: Game -> Game
checks w
  | isJust $ checkWin w = makeWinner w $ GameOver $ checkWin w
  | checkTie w = makeWinner w $ GameOver Nothing
  | otherwise = w

gameAfterTurn :: Game -> (Int, Int) -> Game
gameAfterTurn (Game board player Running) i = Game (board // [(i, Full player)]) (otherPlayer player) Running
gameAfterTurn _ _ = undefined

makeWinner :: Game -> State -> Game
makeWinner w p = w {gameState = p}

getRCofClick :: Float -> Float -> (Int, Int)
getRCofClick x y = (yp y, xp x)
  where
    xp :: Float -> Int
    xp pos
      | pos >= -108 && pos <= 108 = 1
      | pos > 108 = 2
      | otherwise = 0
    yp :: Float -> Int
    yp pos
      | pos >= -108 && pos <= 108 = 1
      | pos > 108 = 0
      | otherwise = 2

otherPlayer :: Player -> Player
otherPlayer PlayerO = PlayerX
otherPlayer PlayerX = PlayerO

isPlayed :: Cell -> Bool
isPlayed Empty = False
isPlayed _ = True

checkTie :: Game -> Bool
checkTie w = all isPlayed (elems $ gameBoard w)

checkWin :: Game -> Maybe Player
checkWin world
  | null onlyGood = Nothing
  | (Full player) : _ <- head onlyGood =
    Just player
  | otherwise = Nothing
  where
    onlyGood = filter isLine $ getLines $ gameBoard world

isLine :: [Cell] -> Bool
isLine line =
  (length grouped == 1)
    && ( case head grouped of
           (Empty : xs) -> False
           (Full _ : xs) -> True
           _ -> undefined
       )
  where
    grouped = group line

getLines :: Board -> [[Cell]]
getLines board = map lineToCells possibleLines
  where
    lineToCells :: [(Int, Int)] -> [Cell]
    lineToCells = map (board !)

possibleLines :: [[(Int, Int)]]
possibleLines =
  [ [(0, 0), (0, 1), (0, 2)],
    [(1, 0), (1, 1), (1, 2)],
    [(2, 0), (2, 1), (2, 2)],
    [(0, 0), (1, 0), (2, 0)],
    [(0, 1), (1, 1), (2, 1)],
    [(0, 2), (1, 2), (2, 2)],
    [(0, 0), (1, 1), (2, 2)],
    [(2, 0), (1, 1), (0, 2)]
  ]
