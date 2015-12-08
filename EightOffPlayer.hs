module EightOffPlayer where

    import EightOff

    findMoves :: EOBoard -> [EOBoard]
    findMoves board@(columns, reserves, foundations) = []

    -- moveKingsToVacantColumns :: EOBoard -> EOBoard
    -- moveKingsToVacantColumns board@(columns, reserves, foundations) =

    -- moveable kings referring to kings that can be moved if there is space


    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board@(columns, reserves, foundations)
        | null (findMoves board) = Nothing


    weightBoard :: EOBoard -> Int
    weightBoard board@(columns, reserves, foundations) = 52 - (length reserves) - sum(map length columns)
