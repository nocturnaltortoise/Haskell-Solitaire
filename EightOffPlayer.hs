module EightOffPlayer where

    import EightOff
    import Data.Ord
    import Data.List
    import Data.Maybe
    import System.Random
    import Debug.Trace

    findMoves :: EOBoard -> [EOBoard]
    findMoves board = movesToFoundations ++ movesToColumns ++ movesToReserves
    --  ++ [movesToReserves] ++ [movesToColumns]
        where movesToFoundations = [toFoundations board | canMoveToFoundations board]
              movesToColumns = [foldr matchCardWithColumns board (findMoveablePredecessors board) | canMoveToColumns board]
              movesToReserves = [moveCardToReserves (fromJust $ bestCardToMove board) board | canMoveToReserves board && (isJust (bestCardToMove board))]
                                                --[moveCardToReserves (head (map head (filter (not.null) columns))) board]

    bestCardToMove :: EOBoard -> Maybe Card
    bestCardToMove board@(columns, reserves, foundations) = traceShow("best card: ", if null list then Nothing else Just bestCardInList) $ if null list then Nothing else Just bestCardInList
        where predecessorCards = findMoveablePredecessors board
              successorCards = findMoveableSuccessors board
              -- || sCard x `elem` predecessorCards
              list = [x | x <- map head (filter (not.null) columns), not(isKing x) && (isAce(sCard x))]
              bestCardInList = sortDescending $ zip(map (\e -> weightBoard $ moveCardToReserves e board) list) list

    sortDescending :: Ord a => [(a,b)] -> b
    sortDescending list = head $ map snd $ sortBy (flip (comparing fst)) list

    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board
        | null (findMoves board) = Nothing
        -- zip the boards with their scores and sort them in descending order?
        | otherwise = Just $ sortDescending $ zip (map weightBoard (findMoves board)) (findMoves board)
                                --head $ map snd $ sortBy (flip (comparing fst))  -- could this map be a fold?
    weightBoard :: EOBoard -> Int
    weightBoard board = weightIndividualBoard board + nextBoard
        where nextBoard = maybe 0 weightIndividualBoard (chooseMove board)
            --   anotherBoard = maybe 0 weightIndividualBoard (chooseMove $ fromJust $ chooseMove board)

            --   anotherBoard = fromMaybe board (chooseMove nextBoard)
    -- add weight of all successor boards?
    -- sum(map weightBoard (findMoves board))

    weightIndividualBoard :: EOBoard -> Int
    weightIndividualBoard (columns, reserves, foundations) = 52 - fromIntegral(length reserves) - sum(map (fromIntegral.length) columns)
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
    playEOGames seed = take 100 $ repeat (eOGame $ eoDeal $ randomNumber)
        where randomNumber = traceShow ("random number: ", head $ randoms (mkStdGen seed)::Int) $ head $ randoms (mkStdGen seed)::Int

    -- eOExpt :: Int -> Int
    -- eOExpt seed = winCount
    --     where winCount =
