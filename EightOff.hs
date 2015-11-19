module EightOff(sCard,
                pCard,
                isAce,
                isKing,
                pack,
                shuffle,
                eoDeal,
                eoBoardToString)
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

    -- does this need an auxiliary function to handle the Maybe?
    sCard :: Card -> Maybe Card
    sCard (pip, suit)
        | pip /= King = Just (succ pip, suit)
        | otherwise = Nothing

    pCard :: Card -> Maybe Card
    pCard (pip, suit)
        | pip /= Ace = Just (pred pip, suit)
        | otherwise = Nothing

    isAce :: Card -> Bool
    isAce (pip,_) = pip == Ace

    isKing :: Card -> Bool
    isKing (pip,_) = pip == King

    pack :: Deck
    pack = suitList Clubs ++ suitList Diamonds ++ suitList Hearts ++ suitList Spades

    suitList :: Suit -> [Card]
    suitList suit = [(pip, suit) | pip <- pipList]
        where pipList = [Ace ..]

    shuffle :: Deck -> Deck
    shuffle a =
        let sortedPack = sortBy (comparing snd) (zip cards randomNumList)
            rng = mkStdGen 334232 --need a way of generating a random seed so shuffles are different every time
            cards = a
            randomNumList = take 50 (randoms rng::[Int])
        in map fst sortedPack

    eoDeal :: EOBoard
    eoDeal =
        let shuffledDeck = shuffle pack
            reserves = take 4 shuffledDeck
            foundations = []
            column = take 6 shuffledDeck -- need a function that splits the deck into the right composition
            columns = [column,column,column,column,column,column,column,column] -- this could be replaced by a call to a function that split the shuffled deck into two lists (or a tuple?), one of 4 cards, and the second of 8 lists of 5
        in (columns,reserves,foundations)


    -- splitDeck :: Deck -> [Deck] -> Deck
    -- -- splitDeck deck columns@[[]] = head deck : head columns
    -- splitDeck deck columns
    --     | length (head columns) <= 6 = (head deck : head columns) ++ splitDeck (tail deck) (tail columns)
    --     | otherwise = splitDeck (tail deck) columns
        -- if length is less than or equal to 6, add to current column
        -- else make new column and cons on to call that produces first column
    -- splitDeck :: Deck -> (Deck,Deck,Deck,Deck,Deck,Deck,Deck,Deck)
    -- splitDeck deck = splitAt 6 snd(splitAt 6 snd(splitAt 6 snd(splitAt 6 snd(splitAt 6 snd(splitAt 6 snd(splitAt 6 snd(splitAt 6 deck)))))))
    -- splitDeck deck = splitDeck(splitAt 6 deck)
    -- splitDeck deck = [(splitAt 6 deck)] ++ splitDeck (drop 6 deck)

    -- splitDeckA :: (Deck,Deck) -> (Deck, Deck)
    -- splitDeckA deck = fst deck ++ (splitDeckA snd(deck))

    splitDeck :: Deck -> [Deck] -> Deck
    splitDeck deck columns
        | length (head columns) <= 6 = (head deck : head columns) ++ splitDeck (tail deck) (tail columns)
        | otherwise = splitDeck (tail deck) (tail columns)
        -- this doesn't make eight separate lists, but a single list of 8 cards
        --
-- *EightOff> splitDeck pack [[],[],[],[],[],[],[],[]]
-- [(Ace,Clubs),(Two,Clubs),(Three,Clubs),(Four,Clubs),(Five,Clubs),(Six,Clubs),(Seven,Clubs),(Eight,Clubs)*** Exception: Prelude.head: empty list

    eoBoardToString :: EOBoard -> String
    eoBoardToString (columns,reserves,foundations) =
        "Foundations: " ++ show foundations
        ++ "Reserves: " ++ show reserves
        ++ "Columns: " ++ show columns

    -- toFoundations :: EOBoard -> EOBoard
    -- toFoundations board =
        -- check the head of the first column for an Ace
            -- if there is an ace, move it to the foundations
                -- now check the new head for a two, if there is one move it to a foundation with an ace of the same suit,
                    -- and check the new head for a three ...
            -- otherwise, check the next column for an ace
