module EightOffPlayer where

    import EightOff
    import Data.Ord
    import Data.List
    import Data.Maybe
    import System.Random
    import Debug.Trace
    import Data.List.Split

    findMoves :: EOBoard -> [EOBoard]
    findMoves board@(columns, reserves, foundations) = movesToFoundations ++ movesToColumns ++ movesToReserves ++ riskyMovesToReserves ++ riskyMovesToReservesUsingSuccessors
        -- | (not.null) (unearthSuccessor board) && (not.null) (unearthAce board) = movesToFoundations ++ movesToColumns ++ movesToReserves ++ riskyMovesToReserves ++ riskyMovesToReservesUsingSuccessors
        -- | otherwise = movesToFoundations ++ movesToColumns ++ movesToReserves
        where movesToFoundations = [toFoundations board | canMoveToFoundations board]
              movesToColumns = [foldr matchCardWithColumns board (findMoveablePredecessors board) | canMoveToColumns board]
              movesToReserves = [moveCardToReserves (fromJust $ bestCardToMove board) board | canMoveToReserves board && (isJust (bestCardToMove board))]
              riskyMovesToReserves = [foldr moveCardToReserves board (unearthAce board) | canMoveToReserves board, (not.null) (unearthAce board)]
              riskyMovesToReservesUsingSuccessors = [foldr moveCardToReserves board (unearthSuccessor board)  | canMoveToReserves board, (not.null) (unearthSuccessor board)]

    bestCardToMove :: EOBoard -> Maybe Card
    bestCardToMove board@(columns, reserves, foundations) = if null list then Nothing else Just bestCardInList
        where predecessorCards = findMoveablePredecessors board
              successorCards = findMoveableSuccessors board
              list = [head x | x <- filter (not.null) columns, (not.null) (tail x), (not.null) successorCards, isAce(head (tail x)) || (head (tail x)) `elem` successorCards]
              bestCardInList = sortDescending $ zip(map (\e -> weightBoard $ moveCardToReserves e board) list) list

    aceInColumn :: Deck -> Bool
    aceInColumn = any isAce

    successorInColumn :: Deck -> Deck -> Bool
    successorInColumn column successors = any (==True) [x `elem` successors | x <- column, (not.null) successors, (not.null) column]

    -- if there's an ace in column, move cards to reserves until ace is the head of the column
    unearthAce :: EOBoard -> Deck
    unearthAce board@(columns, reserves, foundations) = head $ splitWhen (isAce) (chooseColumn[x | x <- columns, aceInColumn x, (not.null) x, (not.null) columns])

    -- need findSuccessors function - EOBoard -> Deck - finds all the successors of the foundations
    findSuccessors :: EOBoard -> Deck
    findSuccessors board@(columns, reserves, foundations) = [card | column <- columns, card <- column, foundation <- foundations, foundationsCard <- foundation, (not.isKing) foundationsCard, card == sCard foundationsCard]

    unearthSuccessor :: EOBoard -> Deck
    unearthSuccessor board@(columns, reserves, foundations) = if null successors then [] else head $ splitOneOf successors (head [x | x <- columns, successorInColumn x successors])
        where successors = findSuccessors board

    chooseColumn :: [Deck] -> Deck
    chooseColumn columns = if (null) (map (length.head.splitWhen (isAce)) columns) then [] else sortAscending (zip (map (length.head.splitWhen (isAce)) columns) columns)

    sortAscending :: Ord a => [(a,b)] -> b
    sortAscending list = head $ map snd $ sortBy (comparing fst) list

    sortDescending :: Ord a => [(a,b)] -> b
    sortDescending list = head $ map snd $ sortBy (flip (comparing fst)) list

    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board@(columns, reserves, foundations)
        | null (findMoves board) = Nothing
        | otherwise = Just $ sortDescending $ zip (map (weightBoard) (findMoves board)) (findMoves board)


    weightBoard :: EOBoard -> Int
    weightBoard board@(columns, reserves, foundations) = sum(map length foundations)
    -- weightBoard :: Int -> EOBoard -> Int
    -- weightBoard depth board@(columns, reserves, foundations)
    --     | depth < 3 = sum(map length foundations)
    --     | otherwise = weightIndividualBoard board
    -- -- + nextBoard
    -- -- + anotherBoard
    --     where nextBoard = maybe 0 (weightBoard (depth+1)) (chooseMove board)
    --           anotherBoard = if isJust $ chooseMove board then weightIndividualBoard $ fromJust $ chooseMove $ fromJust $ chooseMove board else 0

    -- Score a particular board
    weightIndividualBoard :: EOBoard -> Int
    weightIndividualBoard (columns, reserves, foundations) = sum(map length foundations)

    -- Plays an individual game as far as it can and returns a score.
    eOGame :: EOBoard -> Int
    eOGame board@(columns, reserves, foundations)
        | null columns && null reserves = weightBoard board
        | isJust (chooseMove board) = eOGame (fromJust $ chooseMove board)
        | otherwise = weightBoard board

    -- Plays 100 games with 100 random seeds.
    playEOGames :: Int -> [Int]
    playEOGames seed = map (eOGame.eoDeal) (take 100 $ randoms (mkStdGen seed)::[Int])

    -- Finds the average score and the number of wins for a list of scores.
    eOExpt :: Int -> (Int,Float)
    eOExpt seed = (length(filter (==52) scores), (fromIntegral.sum) scores / (fromIntegral.length) scores)
        where scores = playEOGames seed
