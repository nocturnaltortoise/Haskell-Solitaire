module EightOffPlayer where

    import EightOff
    import Data.Ord
    import Data.List
    import Data.Maybe
    import System.Random
    import Debug.Trace
    import Data.List.Split

    findMoves :: EOBoard -> [EOBoard]
    findMoves board = movesToFoundations ++ riskyMovesToReserves ++ movesToColumns ++ movesToReserves
    --  ++ [movesToReserves] ++ [movesToColumns]
        where movesToFoundations = [toFoundations board | canMoveToFoundations board]
              movesToColumns = [foldr matchCardWithColumns board (findMoveablePredecessors board) | canMoveToColumns board]
              movesToReserves = [moveCardToReserves (fromJust $ bestCardToMove board) board | canMoveToReserves board && (isJust (bestCardToMove board))]
              riskyMovesToReserves = [foldr moveCardToReserves board $ unearthAce board | canMoveToReserves board]
                                                --[moveCardToReserves (head (map head (filter (not.null) columns))) board]

    bestCardToMove :: EOBoard -> Maybe Card
    bestCardToMove board@(columns, reserves, foundations) = if null list then Nothing else Just bestCardInList
    -- traceShow("best card: ", if null list then Nothing else Just bestCardInList) $
        where predecessorCards = findMoveablePredecessors board
              successorCards = findMoveableSuccessors board
            --   -- || sCard x `elem` predecessorCards
            --   list = [x | x <- map head (filter (not.null) columns), not(isKing x) && (isAce(sCard x) || sCard x `elem` successorCards || (not(isKing (sCard x)) && (sCard.sCard) x `elem` successorCards))]
              list = [head x | x <- filter (not.null) columns, isAce(head (tail x)) || (head (tail x)) `elem` successorCards]
-- traceShow("list: ", [head x | x <- filter (not.null) columns, isAce(head (tail x)) || (head (tail x)) `elem` successorCards]) $
                --   filter (\e -> if isAce (head (tail e)) then (head (tail e)) else ) (filter (not.null) columns)
              bestCardInList = sortDescending $ zip(map (\e -> weightBoard $ moveCardToReserves e board) list) list

    aceInColumn :: Deck -> Bool
    aceInColumn = any isAce
    --
    unearthAce :: EOBoard -> Deck
    unearthAce board@(columns, reserves, foundations) = head $ splitWhen isAce (chooseColumn[x | x <- columns, aceInColumn x])
            -- if there's an ace in column, move cards to reserves until ace is the head of the column

    chooseColumn :: [Deck] -> Deck
    chooseColumn columns = sortAscending (zip (map (length.head.splitWhen (isAce)) columns) columns)

    sortAscending :: Ord a => [(a,b)] -> b
    sortAscending list = head $ map snd $ sortBy (comparing fst) list

    sortDescending :: Ord a => [(a,b)] -> b
    sortDescending list = head $ map snd $ sortBy (flip (comparing fst)) list

    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board
        | null (findMoves board) = Nothing
        -- zip the boards with their scores and sort them in descending order?
        | otherwise = Just $ sortDescending $ zip (map weightBoard (findMoves board)) (findMoves board)
                                --head $ map snd $ sortBy (flip (comparing fst))  -- could this map be a fold?
    weightBoard :: EOBoard -> Int
    weightBoard board = weightIndividualBoard board
    -- + nextBoard
    -- + anotherBoard
        where nextBoard = maybe 0 weightIndividualBoard (chooseMove board)
              anotherBoard = if isJust $ chooseMove board then weightIndividualBoard $ fromJust $ chooseMove $ fromJust $ chooseMove board else 0
            --   anotherBoard = maybe 0 weightIndividualBoard (chooseMove $ fromJust $ chooseMove board)

            --   anotherBoard = fromMaybe board (chooseMove nextBoard)
    -- add weight of all successor boards?
    -- sum(map weightBoard (findMoves board))

    weightIndividualBoard :: EOBoard -> Int
    weightIndividualBoard (columns, reserves, foundations) = sum(map length foundations)
    --52 - fromIntegral(length reserves) - sum(map (fromIntegral.length) columns)
--sum(map length foundations)
        --52 - fromIntegral(length reserves) - sum(map (fromIntegral.length) columns)

        -- 52 - fromIntegral(length reserves) - sum(map (fromIntegral.length) columns)

    -- getSuccessorBoard :: EOBoard -> Maybe EOBoard
    -- getSuccessorBoard board
    --     | isJust $ chooseMove board = Just (fromJust $ chooseMove board)
    --     | otherwise = Nothing

    eOGame :: EOBoard -> Int
    eOGame board@(columns, reserves, foundations)
        | null columns && null reserves = weightIndividualBoard board
        | isJust (chooseMove board) = weightIndividualBoard $ fromJust $ chooseMove board
        | otherwise = weightIndividualBoard board

    playEOGames :: Int -> [Int]
    playEOGames seed = map (eOGame.eoDeal) (take 100 $ randoms (mkStdGen seed))

    eOExpt :: Int -> (Int,Int)
    eOExpt seed = (length(filter (==52) scores), sum(scores) `div` length(scores))
        where scores = playEOGames seed
