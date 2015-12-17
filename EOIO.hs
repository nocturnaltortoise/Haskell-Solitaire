module EOIO where

{- IO for EO solitaire
   display an EOBoard
   display a list of EOBoards
play a game, displaying successive moves -}



 -- import your solitaire code here

    import EightOff
    import EightOffPlayer
    import Data.Maybe

{-  Data Structures to be imported
 -- playing card data structures

 data Suit = Hearts|Clubs|Diamonds|Spades
             deriving (Eq, Show)

 data Pip = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King
            deriving (Eq,Ord,Show,Enum)

 type Card = (Pip,Suit)

 type Deck = [Card]

 ------------------------------------------------------------------
-- 8 off solitaire data structures

 type EOBoard = (Foundations, Columns, Reserves)

 type Foundations = [Card] -- only need to know top card

 type Columns = [[Card]]

 type Reserves = [Card]
-}

 ----------------------------------------------------------
 -- display an EOBoard
    displayEOB :: EOBoard -> IO String
    displayEOB (columns,reserves,foundations) =
        do
          let colStr = colsToString columns
          putStr "EOBoard\nFoundations  "
          putStrLn (show foundations)
          putStr  "Columns"
          putStr colStr
          putStr "\n\nReserve     "
          putStrLn (show reserves)
          putStr "\n---------------------------------------------\n"
          return ""

    colsToString :: Columns->String -- prepare String to print columns on separate lines
    colsToString cols =
    --   foldr (++) "" ["\n             "++(show col) |col<-cols]
        foldr (++) "" ["\n             "++(show col) |col<-cols]

    -----------------------------------------------------------------------

    -- display a list of EOBoards
     -- @ notation doesn't seem to work correctl
    displayEOBList :: [EOBoard]-> IO String
    displayEOBList list = if (null list) then (return "") else do
                            displayEOB (head list)
                            displayEOBList (tail list)

        -- do
        --     if (null list) then
        --         do
        --             (return "")
        --     else
        --         do
        --             displayEOB (head list)
        --             displayEOBList (tail list)


-----------------------------------------------------------------

 --scoreBoard
 -- score is number of cards on foundations
 -- return a String for display

    scoreBoard :: EOBoard-> String
    scoreBoard (columns, reserves, foundations) = "A LOSS: SCORE  " ++ (show (52 - (length reserves) - (sum(map length columns))))

 -----------------------------------------------------------------------------
 -- play a game given initial board
 -- assuming a fn chooseMove :: EOBoard ->Maybe EOBoard
 -- & that toFoundations is handled outside

    displayEOGame :: EOBoard -> IO String
    displayEOGame board@(columns, reserves, foundations) =
      if ((null columns)&&(null reserves)) -- if cols & reserve empty its a win
         then return "A WIN"
         else
          do
           displayEOB board -- display given board
           let result = chooseMove board
           if (isJust result) then
                   do
                    let nb = fromJust result
                    -- let nb = resMaybe res
                    -- changed to fromJust, which is a library function that does the same thing
                    displayEOGame nb
                  else
                   do
                     let score = scoreBoard board
                     return score

 ------------------------------------------------
 -- Maybe helper
    -- resMaybe :: (Maybe a) -> a
    -- resMaybe (Just x) = x
