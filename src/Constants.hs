module Constants where 
import Graphics.Gloss

windowSize :: (Int, Int)
windowSize = (640, 640)

windowPos :: (Int, Int)
windowPos = (10, 10)

windowTitle :: String
windowTitle = "Tac Tac Toe"

window :: Display
window = InWindow windowTitle windowSize windowPos

fps :: Int
fps = 30
