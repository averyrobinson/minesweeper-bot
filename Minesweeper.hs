module Minesweeper where

import Data.Array
import Data.Maybe
import System.Random

data TileStatus = Unclicked | Clicked | Flagged
    deriving (Eq, Show)
data Tile = Tile { isMine :: Bool, status :: TileStatus, number :: Maybe Int }
    deriving (Show)

type Board = Array (Int, Int) Tile

makeRandomBoard :: Int -> Int -> Int -> IO Board
makeRandomBoard x y numMines = do
    g <- newStdGen
    let plantMine i xs
            | xs !! i == False = take i xs ++ [True] ++ drop (i + 1) xs
            | i + 1 >= length xs = plantMine 0 xs
            | otherwise = plantMine (i + 1) xs
        plantMines 0 xs g = (xs, g)
        plantMines n xs g = let (i, g') = randomR (0,(x*y) - numMines+n - 1) g
                                xs' = plantMine i xs
                            in plantMines (n - 1) xs' g'
        (positions, g') = plantMines numMines (replicate (x * y) False) g
        tiles = map (\b -> Tile b Unclicked Nothing) positions
        board = calcNumbers $
            array ((0,0), (x-1,y-1))
                (zip [(i,j) | i <- [0..x-1], j <- [0..y-1]] tiles)
        emptyIndices = filter (\i -> number (board ! i) == Just 0)
                              (indices board)
        clickIndexIndex = fst $ randomR (0, length emptyIndices - 1) g'
        board' = uncurry click (emptyIndices !! clickIndexIndex) board
    return board'

-- For debugging in GHCi
showBoard :: Board -> String
showBoard board =
    let ((xLower, yLower), (xUpper, yUpper)) = bounds board
        showTile tile
            | status tile == Unclicked = '#'
            | status tile == Flagged   = 'F'
            | isMine tile = 'M'
            | otherwise = head . show . fromJust . number $ tile
    in unlines [ [showTile (board ! (x, y)) | y <- [yLower..yUpper]]
                 | x <- [xLower..xUpper] ]

-- For debugging in GHCi
pb = putStr . showBoard

-- Checks if a mine has been clicked or if all mines are found
gameOver :: Board -> Bool
gameOver board = clickedMine || allFlagged
  where
    clickedMine = any (\i -> isMine (board ! i) && status (board ! i) == Clicked) (indices board)
    allFlagged = all (\i -> if isMine (board ! i)
                                then status (board ! i) == Flagged
                                else status (board ! i) == Clicked)
                     (indices board)

-- Calculates what number a clicked tile should show
calcNumber :: Board -> Int -> Int -> Maybe Int
calcNumber board x y
    | (x, y) `notElem` indices board = Nothing
    | isMine (board ! (x,y)) = Nothing
    | otherwise = Just . length . filter id . map (isMine . (board !)) $ neighbors board x y

-- Populates the board with all the numbers
calcNumbers :: Board -> Board
calcNumbers board = foldr f board (indices board)
  where
    f (x,y) b = updateBoard b x y $ (b ! (x,y)) { number = calcNumber board x y }

-- Calculates the indices for all the neighbors of a tile
neighbors :: Board -> Int -> Int -> [(Int, Int)]
neighbors board x y = 
    let ((xLower, yLower), (xUpper, yUpper)) = bounds board
    in [ (a, b)
       | a <- [x - 1..x + 1],
         b <- [y - 1..y + 1],
         (a, b) /= (x, y),
         a >= xLower,
         a <= xUpper,
         b >= yLower,
         b <= yUpper ]

-- Updates the board when a tile has been clicked
click :: Int -> Int -> Board -> Board
click x y board
    | status (board ! (x,y)) /= Unclicked = board
    | otherwise = let board' = updateBoard board x y $ (board ! (x,y)) { status = Clicked }
                  in if number (board' ! (x,y)) == Just 0 && (not . isMine) (board' ! (x,y))
                     then foldr (\(a,b) acc -> click a b acc) board' (neighbors board' x y)
                     else board'

-- Updates the board when a tile has been flagged
flag :: Int -> Int -> Board -> Board
flag x y board
    | status' == Unclicked = updateBoard board x y $ (board ! (x,y)) { status = Flagged   }
    | status' == Flagged   = updateBoard board x y $ (board ! (x,y)) { status = Unclicked }
    | status' == Clicked   = board
      where
        status' = status $ board ! (x,y)

-- Changes one tile
updateBoard :: Board -> Int -> Int -> Tile -> Board
updateBoard board x y tile = board // [((x,y), tile)]
