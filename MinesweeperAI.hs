module MinesweeperAI where

import Minesweeper
import Control.Monad.State
import Data.Array
import Data.List
import Data.Function

-- Calculates how many unflagged mines are near a tile (without cheating, of
-- course)
minesRem :: Int -> Int -> State Board Int
minesRem x y = do
    board <- get
    case status (board ! (x,y)) of
        Clicked -> case number (board ! (x,y)) of
                        Just n -> do
                            fns <- flaggedNeighbors x y
                            return $ n - length fns
                        Nothing -> error "called minesRem on a tile with no number"
        _ -> error "called minesRem on an unclicked or flagged tile"

-- Returns a list of all valid indices for a board
getIndices :: State Board [(Int, Int)]
getIndices = get >>= return . indices

-- Gets a tile given index
getTile :: Int -> Int -> State Board Tile
getTile x y = do
    board <- get
    return $ board ! (x,y)

-- Gets the indices of all the neighbors of a tile
getNeighbors :: Int -> Int -> State Board [(Int, Int)]
getNeighbors x y = do
    board <- get
    return $ neighbors board x y

-- Checks if the tile at the given index is a nonzero number
isNumber :: Int -> Int -> State Board Bool
isNumber x y = do
    t <- getTile x y
    return $ status t == Clicked && (case number t of
                                         Nothing -> False
                                         Just n -> n /= 0)

-- Gets the indices of all number neighbors of a tile
numberNeighbors :: Int -> Int -> State Board [(Int, Int)]
numberNeighbors x y = getNeighbors x y >>= filterM (uncurry isNumber)

-- Checks if a tile is flagged
isFlagged :: Int -> Int -> State Board Bool
isFlagged x y = do
    t <- getTile x y
    return $ status t == Flagged

-- Gets the indices of all flagged neighbors
flaggedNeighbors :: Int -> Int -> State Board [(Int, Int)] 
flaggedNeighbors x y = getNeighbors x y >>= filterM (uncurry isFlagged)

-- Checks if a tile is unclicked
isUnclicked :: Int -> Int -> State Board Bool
isUnclicked x y = do
    t <- getTile x y
    return $ status t == Unclicked

-- Gets the indices of all unclicked neighbors
unclickedNeighbors :: Int -> Int -> State Board [(Int, Int)]
unclickedNeighbors x y = getNeighbors x y >>= filterM (uncurry isUnclicked)

-- Gets the indices of all numbers that have unclicked neighbors that are also
-- neighbors of the tile at the given index
relevantNumbers :: Int -> Int -> State Board [(Int, Int)]
relevantNumbers x y = unclickedNeighbors x y >>=
                      fmap concat . mapM (uncurry numberNeighbors) >>=
                      return . (delete (x,y))

-- Do one step of logic
logicalStep :: State Board Bool
logicalStep = orM [flagRule1, clickRule1, flagRule2, clickRule2]

-- Flagging rules. See rules.txt for English descriptions.

flagRule1 :: State Board Bool
flagRule1 = do
    indices' <- getIndices
    is <- filterM (uncurry isNumber) indices'
    let bools = for is $ \(x,y) -> do
        r <- minesRem x y
        fns <- flaggedNeighbors x y
        uns <- unclickedNeighbors x y
        if r == length fns + length uns && length uns /= 0
            then forM uns (\(a,b) -> modify (flag a b)) >> return True
            else return False
    orM bools

flagRule2 :: State Board Bool
flagRule2 = do
    indices' <- getIndices
    is <- filterM (uncurry isNumber) indices'
    let bools = for is $ \(x,y) -> do
        r <- minesRem x y
        if r == 0
            then return False
            else do
                uns <- unclickedNeighbors x y
                all_bs <- fmap (delete (x,y)) $ relevantNumbers x y
                let bss = powerset all_bs
                bss' <- flip filterM bss $ \bs -> do
                    allbuns <- fmap (foldr union []) $ mapM (uncurry unclickedNeighbors) bs
                    brTotal <- fmap sum $ mapM (uncurry minesRem) bs
                    return $ all (`elem` uns) allbuns
                          && (not . null $ uns \\ allbuns)
                          && r - brTotal == length (uns \\ allbuns)
                case bss' of
                    (bs:_) -> do
                        allbuns <- fmap (foldr union []) $ mapM (uncurry unclickedNeighbors) bs
                        forM (uns \\ allbuns) (\(a,b) -> modify (flag a b))
                        return True
                    [] -> return False
    orM bools

-- Clicking rules. See rules.txt for English descriptions.

clickRule1 :: State Board Bool
clickRule1 = do
    indices' <- getIndices
    is <- filterM (uncurry isNumber) indices'
    let bools = for is $ \(x,y) -> do
        r <- minesRem x y
        if r == 0
            then do
                uns <- unclickedNeighbors x y
                forM uns (\(a,b) -> modify (click a b))
                return (length uns /= 0)
            else return False
    orM bools

clickRule2 :: State Board Bool
clickRule2 = do
    indices' <- getIndices
    is <- filterM (uncurry isNumber) indices'
    let bools = for is $ \(x,y) -> do
        r <- minesRem x y
        if r == 0
            then return False
            else do
                uns <- unclickedNeighbors x y
                all_bs <- fmap (delete (x,y)) $ relevantNumbers x y
                let bss = powerset all_bs
                bss' <- flip filterM bss $ \bs -> do
                    bunss <- mapM (uncurry unclickedNeighbors) bs
                    let allbuns = foldr union [] bunss
                    rhs <- fmap sum $ forM bs $ \(i,j) -> do
                        brem <- minesRem i j
                        buns <- unclickedNeighbors i j
                        return $ brem - length (buns \\ uns)
                    return $ (not . null $ uns \\ allbuns)
                          && length allbuns == sum (map length bunss)
                          && r == rhs
                case bss' of
                    [] -> return False
                    (bs:_) -> do
                        allbuns <- fmap (foldr union []) $ mapM (uncurry unclickedNeighbors) bs
                        forM (uns \\ allbuns) (\(a,b) -> modify (click a b))
                        return True
    orM bools

-- Misc

-- Only evaluates monads until it gets True
orM :: Monad m => [m Bool] -> m Bool
orM = foldM (\b n -> if b then return True else n) False

for :: [a] -> (a -> b) -> [b]
for = flip map

-- Sorts the resulting powerset so that the algorithm favors easier to compute
-- things
powerset :: [a] -> [[a]]
powerset = sortBy (compare `on` length) . filterM (const [False, True])
