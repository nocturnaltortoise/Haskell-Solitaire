module EightOffPlayer where

    import EightOff
    import Data.Ord
    import Data.List

    findMoves :: EOBoard -> [EOBoard]
    findMoves board@(columns, reserves, foundations)
        | canMoveToColumns board = [foldr (matchCardWithColumns) board (findMoveablePredecessors board)]
        | canMove board = [toFoundations board]
        | canMoveKing board = map (moveKingToNewColumn board) (findMoveableKings board)
        -- | canMoveToReserves board = moveCardToReserves
            --move cards that expose cards that can be moved elsewhere
                -- i.e. move cards that produce a board where canMove || canMoveToColumns || canMoveKing
        | otherwise = []
        -- | canMoveToReserves board =


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
        | otherwise = Just $ head $ map snd $ sortBy (comparing fst) $ zip (map weightBoard (findMoves board)) (findMoves board)
                                                                        -- could this map be a fold?
    weightBoard :: EOBoard -> Int
    weightBoard (columns, reserves, _) = 52 - (length reserves) - sum(map length columns)
    -- add weight of all successor boards?
    -- sum(map weightBoard (findMoves board))
