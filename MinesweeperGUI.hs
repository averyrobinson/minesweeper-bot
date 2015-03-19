import Data.Array
import Data.Monoid (mconcat)
import Graphics.Gloss.Interface.IO.Game
import Minesweeper
import MinesweeperAI
import Control.Monad.State

-- This file is really boring. I think you'll be able to understand it by
-- reading the Gloss documentation if you're interested.

xMax = 16
yMax = 16
mines = 40

main = do
    startBoard <- makeRandomBoard xMax yMax mines
    playIO
        (InWindow "Minesweeper" (yMax * 25 + 50, xMax * 25 + 50) (25, 25))
        (greyN 0.95)
        4
        (startBoard, True)
        drawBoard
        handleInput
        stepGame

drawBoard :: (Board, Bool) -> IO Picture
drawBoard (board, _) = return tiles
  where
    tiles = mconcat
        [ translate (fromIntegral $ j * 25 - (yMax * 25 `div` 2))
                    (fromIntegral $ i * 25 - (xMax * 25 `div` 2)) $ scale 0.10 0.10 $ 
              case status tile of
                Unclicked -> color (greyN 0.1) (text "#")
                Flagged -> translate 50 50 $ color red (thickCircle 0 100)
                Clicked -> case number tile of
                               Just 0 -> color (greyN 0.5) (text "0")
                               Just 1 -> color blue (text "1")
                               Just 2 -> color green (text "2")
                               Just 3 -> color orange (text "3")
                               Just 4 -> color azure (text "4")
                               Just 5 -> color (addColors black red) (text "5")
                               Just 6 -> color cyan (text "6")
                               Just 7 -> color violet (text "7")
                               Just 8 -> color black (text "8")
                               Nothing -> text "M"
        | (i,j) <- indices board
        , let tile = board ! (i,j) ]

handleInput :: Event -> (Board, Bool) -> IO (Board, Bool)
handleInput _ b = return b

stepGame :: Float -> (Board, Bool) -> IO (Board, Bool)
stepGame _ (b, continue)
    | continue =
        if gameOver b
            then putStrLn "Done." >> return (b, False)
            else let (progress, b') = runState logicalStep b
                 in if progress
                    then return (b', True)
                    else do
                        putStrLn "Can't deduce anything else."
                        return (b, False)
    | otherwise = return (b, False)
