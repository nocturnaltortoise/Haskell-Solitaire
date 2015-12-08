module EightOff(pack,
                shuffle,
                eoDeal,
                toFoundations,
                Suit,
                Pip,
                Card,
                Deck,
                Columns,
                Reserves,
                Foundations,
                EOBoard)
where

    import System.Random
    import Data.List
    import Data.Ord

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
    sCard :: Card -> Card
    sCard (pip, suit)
        | pip /= King = (succ pip, suit)
        | otherwise = error "Can't get a successor for a King"

    -- Takes a card and returns the previous card in the suit, unless it's an Ace
    -- in which case there is no valid previous card so return Nothing
    pCard :: Card -> Card
    pCard (pip, suit)
        | pip /= Ace = (pred pip, suit)
        | otherwise = error "Can't get a predecessor for an Ace"

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
    -- this needs to take a seed so it can be used to generate serveral boards
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
    --Unfortunately printing show "\n" doesn't make a newline, that would need IO
    -- Deprecated - use EOIO displayEOB
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
    toFoundationsA board@(columns,reserves,_) = (new_columns, new_reserves, new_foundations)
        -- call moveToFoundations with the current board state and the cards to be moved
        where (_,_,new_foundations) = foldr moveToFoundations board (topAces ++ successorCards)
              -- check whether there are any aces or successor cards (cards that have been moved) in the heads of the columns,
              -- if there are, return the tail (remove those cards) otherwise leave the cards there
              new_columns = filter (not.null)
                                (map (\e -> if isAce (head e) || any (elem (head e)) [successorCards] then tail e
                                    else e) columns)
              -- filter out the aces and successor cards from the reserves
              new_reserves = filter (\e -> not(isAce e|| e `elem` successorCards)) reserves
              topAces = findMoveableAces board
              successorCards = findMoveableSuccessors board

    -- Find aces in the heads of the columns or the reserves.
    findMoveableAces :: EOBoard -> Deck
    findMoveableAces (columns, reserves,_) = filter isAce ((map head columns) ++ reserves)

    -- Find the successor cards that are in the heads of the columns or the reserves
    findMoveableSuccessors :: EOBoard -> Deck
    findMoveableSuccessors (columns, reserves, foundations) =
        -- check whether the head of the foundation is a king, if so, return the head, not the
        -- successor, as a King does not have a successor. If the head isn't a king, find the
        -- successor to the head. Then filter out the successors to all the heads of the foundations,
        -- checking whether they are in the heads of the columns or the reserves (moveable).
        filter (\e -> e `elem` ((map head columns) ++ reserves))
            (map (\e -> if isKing (head e)
                then head e else (sCard.head) e) foundations)

    -- Checks whether there are any cards that can be moved to the foundations.
    canMove :: EOBoard -> Bool
    canMove board = (not.null) (findMoveableAces board)
                        || (not.null) (findMoveableSuccessors board)

    -- Moves a card to the foundations and returns the resulting EOBoard
    moveToFoundations :: Card -> EOBoard -> EOBoard
    moveToFoundations card (columns,reserves,foundations) = (columns, reserves, newFoundations)
        where newFoundations = matchCardWithFoundations card foundations

    -- Check if two cards are of the same suit.
    sameSuit :: Card -> Card -> Bool
    sameSuit (_,suitOne) (_,suitTwo) = suitOne == suitTwo

    -- Match a card with the correct foundation if it already has an ace in it, or create a new one.
    matchCardWithFoundations :: Card -> Foundations -> Foundations
    matchCardWithFoundations card foundations
        | isAce card = [card] : foundations
        | otherwise = map (\e -> if sameSuit card (head e) then card : e else e) foundations
