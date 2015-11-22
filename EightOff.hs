module EightOff(sCard,
                pCard,
                isAce,
                isKing,
                pack,
                shuffle,
                eoDeal,
                toFoundations)
where

    import System.Random
    import Data.List
    import Data.Ord
    import Data.Maybe

    data Suit = Clubs | Diamonds | Hearts | Spades
        deriving (Eq, Ord, Show)
    data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
        deriving (Eq, Ord, Show, Enum)
    type Card = (Pip, Suit)
    type Deck = [Card]
    type Columns = [Deck]
    type Reserves = [Card]
    type Foundations = [Deck]
    type EOBoard = (Columns, Reserves, Foundations)

    -- Takes a card and returns the next card in the suit, unless it's a King
    -- in which case there is no valid next card so return Nothing
    sCard :: Card -> Maybe Card
    sCard (pip, suit)
        | pip /= King = Just (succ pip, suit)
        | otherwise = Nothing

    -- Takes a card and returns the previous card in the suit, unless it's an Ace
    -- in which case there is no valid previous card so return Nothing
    pCard :: Card -> Maybe Card
    pCard (pip, suit)
        | pip /= Ace = Just (pred pip, suit)
        | otherwise = Nothing

    -- Takes a card and returns whether it's an Ace
    isAce :: Card -> Bool
    isAce (pip,_) = pip == Ace

    -- Takes a card and returns whether it's a King
    isKing :: Card -> Bool
    isKing (pip,_) = pip == King

    -- Generates a list of all the cards
    pack :: Deck
    pack = suitList Clubs ++ suitList Diamonds ++ suitList Hearts ++ suitList Spades

    -- Generates all the cards in a suit
    suitList :: Suit -> [Card]
    suitList suit = [(pip, suit) | pip <- pipList]
        where pipList = [Ace ..]

    -- Shuffles a set of cards
    shuffle :: Deck
    shuffle =
        let sortedPack = sortBy (comparing snd) (zip cards randomNumList)
            rng = mkStdGen 33423234
            -- arbitrary seed, shuffling not really random.
            cards = pack
            randomNumList = take 52 (randoms rng::[Int])
        in map fst sortedPack

    -- Creates a Board from the shuffled pack.
    eoDeal :: EOBoard
    eoDeal =
        let shuffledDeck = shuffle -- Shuffle the pack of cards
            reserves = take 4 shuffledDeck -- Take four out for the reserves
            foundations = [] -- Make an empty list for the starting foundations
            columns = splitDeck (drop 4 shuffledDeck) -- Split the remaining cards into 8 columns of 6
        in (columns,reserves,foundations)

    -- Recursively split a deck of cards into lists of 6
    splitDeck :: Deck -> [Deck]
    splitDeck [] = []
    splitDeck deck = h : splitDeck t
        where (h,t) = splitAt 6 deck

    --Helper function that makes the EOBoard a little easier to read.
    eoBoardToString :: EOBoard -> String
    eoBoardToString (columns,reserves,foundations) =
        "Columns: " ++ show columns
        ++ "Reserves: " ++ show reserves ++
        "Foundations: " ++ show foundations

    -- Recursively calls a helper function until there are no more legal moves.
    toFoundations :: EOBoard -> EOBoard
    toFoundations board
        | canMove board = toFoundations (toFoundationsA board)
        | otherwise = board

    -- Helper function that moves all the cards that can be moved without recursing down the columns
    toFoundationsA :: EOBoard -> EOBoard
    toFoundationsA board@(columns,reserves,foundations) = (new_columns, new_reserves, new_foundations)
        where (_,_,new_foundations) = foldr moveToFoundations board (topAces ++ successorCards)
              new_columns = filter (not.null) (filter (not . isAce . head) columns) -- this needs to remove only cards that have been moved
              new_reserves = filter (not . isAce) reserves -- similarly
              topAces = findMoveableAces board
              successorCards = findMoveableSuccessors board
            --   topAces = filter isAce (reserves ++ map head (filter (not . null) columns))
            --   successorCards = findSuccessors (filter isAce ((map head (columns ++ foundations)) ++ reserves))
              -- perhaps this should call a function that returns a newly constructed board - with the successors removed from the columns and added to the foundations

              -- need to work out which cards we can move, and then move them, and then remove them from the columns and reserves
              -- every card's unique so we can literally filter them all out of both columns and reserves - might need a helper function for that

    -- Find aces in the heads of the columns or the reserves.
    findMoveableAces :: EOBoard -> Deck
    findMoveableAces (columns, reserves, foundations) = filter isAce ((map head columns) ++ reserves)

    -- Find the successor cards that are in the heads of the columns or the reserves
    findMoveableSuccessors :: EOBoard -> Deck
    findMoveableSuccessors (columns, reserves, foundations) = filter (\e -> any (elem e) (columns ++ [reserves])) (map (fromJust . sCard . head) foundations) -- need to check whether successors are in the columns

    -- Checks whether there are any cards that can be moved to the foundations.
    canMove :: EOBoard -> Bool
    canMove board@(columns,reserves,foundations) = (not.null) (findMoveableAces board) || (not.null) (findMoveableSuccessors board)

    -- Finds successors cards of a list of cards.
    findSuccessors :: Deck -> Deck
    findSuccessors [] = []
    findSuccessors [card]
        | isKing card = []
        | otherwise = [(fromJust . sCard) card]
    findSuccessors (h:t) = (fromJust . sCard) h : findSuccessors t

    -- Returns whether a successor of a card exists
    isSuccessor :: Card -> Bool
    isSuccessor = isJust . sCard

    -- Moves a card to the foundations and returns the resulting EOBoard
    moveToFoundations :: Card -> EOBoard -> EOBoard
    moveToFoundations card (columns,reserves,foundations) = (columns, reserves, newFoundations)
        where newFoundations = if null foundations then [[card]] else matchCardWithFoundations card foundations

    -- Check if two cards are of the same suit.
    sameSuit :: Card -> Card -> Bool
    sameSuit (_,suitOne) (_,suitTwo) = suitOne == suitTwo

    -- Match a card with the correct foundation if it already has an ace in it, or create a new one.
    matchCardWithFoundations :: Card -> Foundations -> Foundations
    matchCardWithFoundations card foundations
        | isAce card = [card] : foundations
        | otherwise = map (\e -> if sameSuit card (head e) then card : e else e) foundations
