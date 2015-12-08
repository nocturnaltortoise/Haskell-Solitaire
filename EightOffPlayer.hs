module EightOffPlayer where

    import EightOff
    import Data.Ord
    import Data.List

    findMoves :: EOBoard -> [EOBoard]
    findMoves board@(columns, reserves, foundations) = []

    -- findMove board
    -- if can move king = findMove (moveKing)
    -- if can move to reserves = findMove (move to reserves)
    -- if can move to foundations = findMove (move to foundations)
    -- otherwise = board

    -- moveKingsToVacantColumns :: EOBoard -> EOBoard
    -- moveKingsToVacantColumns board@(columns, reserves, foundations) =

    -- moveable kings referring to kings that can be moved if there is space

    moveCardToReserves :: Card -> EOBoard -> EOBoard
    moveCardToReserves card (columns, reserves, foundations) = (new_columns, new_reserves, foundations)
        where new_columns = (filter (card `notElem`) columns)
              new_reserves = card : reserves

    moveKingToNewColumn :: Card -> EOBoard -> EOBoard
    moveKingToNewColumn card board@(columns, reserves, foundations)
        | canMoveKing board = (new_columns, reserves, foundations)
        | otherwise = board
            where new_columns = (filter (card `notElem`) columns) ++ [[card]]

    matchCardWithColumns :: Card -> Columns -> Columns
    matchCardWithColumns card columns
        | isKing card = columns ++ [[card]]
        | otherwise = map (\e -> if (sameSuit card (head e)) && pCard (head e) == card then card : e else e) columns

    -- need to be able to move successors of cards on columns to those columns
    -- moveCardToColumn :: Card -> EOBoard -> Columns
    -- moveCardToColumn card board@(columns, reserves, foundations) = filter (card `notElem`) columns

    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board
        | null (findMoves board) = Nothing
        -- | otherwise = (max(foldr weightBoard 0 findMoves board))
        -- zip the boards with their scores and sort them in descending order?
        | otherwise = Just $ head $ map snd $ sortBy (comparing fst) $ zip (map weightBoard (findMoves board)) (findMoves board)
                                                                        -- could this map be a fold?
    weightBoard :: EOBoard -> Int
    weightBoard (columns, reserves, _) = 52 - (length reserves) - sum(map length columns)
    -- add weight of all successor boards?
    -- sum(map weightBoard (findMoves board))
