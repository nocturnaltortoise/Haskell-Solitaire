module EightOffPlayer where

    import EightOff
    import Data.Ord
    import Data.List
    import Data.Maybe

    findMoves :: EOBoard -> [EOBoard]
    findMoves board@(columns, reserves, foundations)
        -- | canMoveToFoundations board = [movesToFoundations]
        -- | canMoveToFoundationsToReserves board = [movesToReserves]
        | canMoveToFoundations board = [movesToFoundations]
        | canMoveToColumns board = [movesToColumns]
        -- | otherwise = []
        -- | canMoveToFoundations board && canMoveToColumns board && canMoveToReserves board = [movesToFoundations, movesToColumns, movesToReserves]
        -- | canMoveToFoundations board && canMoveToColumns board = [movesToFoundations, movesToColumns]
        | canMoveToReserves board = [movesToReserves]
        | otherwise = []
        where movesToFoundations = toFoundations board
              movesToColumns = foldr (matchCardWithColumns) board (findMoveablePredecessors board)
              movesToReserves = if isJust $ bestCardToMove board then moveCardToReserves (fromJust $ bestCardToMove board) board else moveCardToReserves (head(map head columns)) board  --very primitive - will just fill reserves
        -- | canMoveToFoundations board = findMoves $ toFoundations board
        -- | canMoveToFoundationsToColumns board = findMoves $ foldr (matchCardWithColumns) board (findMoveablePredecessors board)
        -- -- | canMoveToFoundationsKing board = map (moveKingToNewColumn board) (findMoveableKings board)
        -- -- | canMoveToFoundationsToReserves board = moveCardToReserves
        --     --move cards that expose cards that can be moved elsewhere
        --         -- i.e. move cards that produce a board where canMoveToFoundations || canMoveToFoundationsToColumns || canMoveToFoundationsKing
        -- | otherwise = []
        -- | canMoveToFoundationsToReserves board =

    bestCardToMove :: EOBoard -> Maybe Card
    bestCardToMove board@(columns, reserves, foundations) = if null list then Nothing else Just bestCardInList
        where predecessorCards = map (\e -> if isAce e then e else pCard e) (map head columns)
              successorCards = findMoveableSuccessors board
              list = [x | x <- (map head columns), isAce(sCard x) || sCard x `elem` predecessorCards || sCard x `elem` successorCards]
              bestCardInList = sortDescending $ zip(map (\e -> weightBoard $ moveCardToReserves e board) list) list


    sortDescending :: Ord a => [(a,b)] -> b
    sortDescending list = head $ map snd $ sortBy (flip (comparing fst)) list
    -- findMove board
    -- if can move king = findMove (moveKing)
    -- if can move to reserves = findMove (move to reserves)
    -- if can move to foundations = findMove (move to foundations)
    -- otherwise = board

    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board
        | null (findMoves board) = Nothing
        -- | otherwise = (max(foldr weightBoard 0 findMoves board))
        -- zip the boards with their scores and sort them in descending order?
        | otherwise = Just $ sortDescending $ zip (map weightBoard (findMoves board)) (findMoves board)
                                --head $ map snd $ sortBy (flip (comparing fst))                                        -- could this map be a fold?
    weightBoard :: EOBoard -> Int
    weightBoard board@(columns, reserves, _) = 52 - fromIntegral(length reserves) - sum(map (fromIntegral.length) columns) + weightBoard nextBoard + weightBoard anotherBoard
        where nextBoard = fromMaybe board (chooseMove board)
              anotherBoard = fromMaybe board (chooseMove nextBoard)
    -- add weight of all successor boards?
    -- sum(map weightBoard (findMoves board))
