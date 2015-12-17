module EightOffPlayer where

    import EightOff
    import Data.Ord
    import Data.List
    import Data.Maybe
    import System.Random
    import Debug.Trace
    import Data.List.Split

    findMoves :: EOBoard -> [EOBoard]
    findMoves board@(columns, reserves, foundations) = movesToFoundations ++ movesToColumns ++ movesToReserves ++ movesToReservesUsingSuccessors
        where movesToFoundations = [toFoundations board | canMoveToFoundations board]
              movesToColumns = [foldr matchCardWithColumns board (findMoveablePredecessors board) | canMoveToColumns board]
              movesToReserves = [foldr moveCardToReserves board (unearthAce board) | canMoveToReserves board, (not.null) (unearthAce board)]
              movesToReservesUsingSuccessors = [foldr moveCardToReserves board (unearthSuccessor board)  | canMoveToReserves board, (not.null) (unearthSuccessor board)]

    -- Returns true if there is an Ace in a given column
    aceInColumn :: Deck -> Bool
    aceInColumn = any isAce

    -- Returns true if there is a successor to the foundations in a given column.
    -- Basically just checks whether any elements of one list are equal to any elements of the other list.
    successorInColumn :: Deck -> Deck -> Bool
    successorInColumn column successors = any (==True) [x `elem` successors | x <- column, (not.null) successors, (not.null) column]

    -- if there's an ace in column, move cards to reserves until ace is the head of the column
    unearthAce :: EOBoard -> Deck
    unearthAce board@(columns, reserves, foundations) = head $ splitWhen (isAce) (chooseColumn[x | x <- columns, aceInColumn x, (not.null) x, (not.null) columns])

    -- Finds successors to the foundations if they exist in the columns
    -- Note - these aren't moveable successors necessarily - that's what findMoveableSuccessors is for
    findSuccessors :: EOBoard -> Deck
    findSuccessors board@(columns, reserves, foundations) = [card | column <- columns, card <- column, foundation <- foundations, foundationsCard <- foundation, (not.isKing) foundationsCard, card == sCard foundationsCard]

    -- Returns the cards needed to be moved to uncover a successor to the foundations.
    unearthSuccessor :: EOBoard -> Deck
    unearthSuccessor board@(columns, reserves, foundations) = if null successors then [] else head $ splitOneOf successors (head [x | x <- columns, successorInColumn x successors])
        where successors = findSuccessors board

    -- chooseSuccessorColumn :: [Deck] -> Deck -> Deck
    -- chooseSuccessorColumn columns successors = zip (map (length.(splitOneOf successors) [x | x <- columns, successorInColumn x successors])) columns

    --Chooses the column which has an Ace in it, and has the smallest number of cards before that Ace.
    chooseColumn :: [Deck] -> Deck
    chooseColumn columns = if (null) (map (length.head.splitWhen (isAce)) columns) then [] else sortAscending (zip (map (length.head.splitWhen (isAce)) columns) columns)

    -- Take a list of tuples, sort them in ascending order on the first value and return the second.
    sortAscending :: Ord a => [(a,b)] -> b
    sortAscending list = head $ map snd $ sortBy (comparing fst) list

    -- Take a list of tuples, sort them in descending order on the first value and return the second.
    sortDescending :: Ord a => [(a,b)] -> b
    sortDescending list = head $ map snd $ sortBy (flip (comparing fst)) list

    -- Chooses the best move based on the score of every possible move
    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board@(columns, reserves, foundations)
        | null (findMoves board) = Nothing
        | otherwise = Just $ sortDescending $ zip (map (weightBoard) (findMoves board)) (findMoves board)
                            -- zip the scores of each board with each board, and sort them so the best score is first

    --Weights a particular board
    weightBoard :: EOBoard -> Int
    weightBoard board@(columns, reserves, foundations) = sum(map length foundations)

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

    runExperiment :: Int -> [(Int,Float)]
    runExperiment seed = map (eOExpt) (take 10 $ randoms (mkStdGen seed)::[Int])
