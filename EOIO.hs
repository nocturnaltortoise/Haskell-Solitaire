module EOIO where
 
{- IO for EO solitaire 
   display an EOBoard
   display a list of EOBoards
play a game, displaying successive moves -}



 -- import your solitaire code here
 
 import Solitaire1 
 import Solitaire2
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
 
 displayEOB (fnds,cols,res) = do
  let colStr = colsToString cols
  putStr "EOBoard\nFoundations  "
  putStrLn (show fnds)
  putStr  "Columns"
  putStr colStr
  putStr "\n\nReserve     "
  putStrLn (show res)
  putStr "\n---------------------------------------------\n"
  return ""

 colsToString :: Columns->String -- prepare String to print columns on separate lines
 
 colsToString cols =
  foldr (++) "" ["\n             "++(show col) |col<-cols]
  
-----------------------------------------------------------------------

-- display a list of EOBoards  

 displayEOBList :: [EOBoard]-> IO String
 
 displayEOBList eobl =  -- @ notation doesn't seem to work correctly
  do
   if (null eobl) then do (return "")
                  else do
                        displayEOB (head eobl)
                        displayEOBList (tail eobl)
 
   
-----------------------------------------------------------------

 --scoreBoard
 -- score is number of cards on foundations
 -- return a String for display
 
 scoreBoard :: EOBoard-> String 
 scoreBoard (fnds, cols, res) = "A LOSS: SCORE  " ++ (show (52- (length res) - (foldr (+) 0 (map length cols))))      

 -----------------------------------------------------------------------------
 -- play a game given initial board
 -- assuming a fn chooseMove :: EOBoard ->Maybe EOBoard
 -- & that toFoundations is handled outside
 
 displayEOGame :: EOBoard ->IO String
 
 displayEOGame b = do
  let (fnds,cols,res) = b -- apparently can't do this with @
  if ((null cols)&&(null res)) -- if cols & reserve empty its a win
     then return "A WIN"
     else 
      do
       displayEOB b -- display given board
       let res = chooseMove b
       if (isJust res) then
               do
                let nb = resMaybe res
                displayEOGame nb
              else
               do
                 let score = scoreBoard b
                 return score
 
 ------------------------------------------------  
 -- Maybe helper                
 resMaybe :: (Maybe a) -> a
 resMaybe (Just x) = x 
 
    