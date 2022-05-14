module Rendering where

import Constants
import Data.Maybe
import GHC.Arr (assocs)
import Game
import Graphics.Gloss

oneThird :: Float
oneThird = 216

drawBoard :: [Picture]
drawBoard =
  map
    line
    [ [(oneThird, 0), (oneThird, 640)],
      [(oneThird * 2, 0), (oneThird * 2, 640)],
      [(0, oneThird), (640, oneThird)],
      [(0, oneThird * 2), (640, oneThird * 2)]
    ]

drawTurns :: Game -> [Picture]
drawTurns g = mapMaybe picsFromCells $ assocs $ gameBoard g

picsFromCells :: ((Int, Int), Cell) -> Maybe Picture
picsFromCells ((_, _), Empty) = Nothing
picsFromCells ((r, c), Full player) =
  case player of
    PlayerO -> Just $ moveTo r c drawO
    PlayerX -> Just $ moveTo r c drawX

moveTo :: Int -> Int -> Picture -> Picture
moveTo r' c' = translate (c * oneThird) (r * (-oneThird))
  where
    c = fromIntegral c'
    r = fromIntegral r'

drawX :: Picture
drawX = color blue $ translate 100 535 $ pictures [line [(-75, -75), (75, 75)], line [(-75, 75), (75, -75)]]

drawO :: Picture
drawO = color red $ translate 100 535 $ circle 95

moveToCenter :: Picture -> Picture
moveToCenter = translate (-320) (-320)

drawEverything :: Game -> Picture
drawEverything world = moveToCenter $ pictures $ drawBoard ++ drawTurns world

renderGame :: Game -> Picture
renderGame w@(Game _ _ Running) = drawEverything w
renderGame w@(Game _ _ (GameOver player))
  | isJust player = colorFromPlayer pic $ fromJust player
  | otherwise = color green pic
    where pic = drawEverything w
          colorFromPlayer :: Picture -> Player -> Picture
          colorFromPlayer pic player'
            | PlayerO <- player' = color red pic
            | otherwise = color blue pic