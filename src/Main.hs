import Data.Sequence
import Data.Foldable

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Rank = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK deriving (Show, Enum, Eq)

data Card = Card Rank Suit deriving (Show)

data StackType = Foundation | Row deriving (Show)
data Stack = Stack [Card] StackType deriving (Show)

data Board = Board [Stack] deriving (Show)

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

cards :: Stack -> [Card]
cards (Stack cards stackType) = cards

stackType :: Stack -> StackType
stackType (Stack cards stackType) = stackType


-- isLegalMove :: Stack -> Stack -> Bool
-- isLegalMove (Row (x:_)) (Row xs) = isLegalMoveRow x xs
-- isLegalMove (Row (x:_)) (Foundation xs) = isLegalMoveFoundation x xs
-- isLegalMove (Foundation (x:_)) (Row xs) = isLegalMoveRow x xs
-- isLegalMove _ _ = False

-- isLegalMoveRow :: Card -> [Card] -> Bool
-- isLegalMoveRow _ [] = True
-- isLegalMoveRow (Card RA _) _ = False
-- isLegalMoveRow (Card RK _) (x:_) = False
-- isLegalMoveRow card (topRowCard:_) = succ (rank card) == rank topRowCard

-- isLegalMoveFoundation :: Card -> [Card] -> Bool
-- isLegalMoveFoundation _ ((Card RK _):_) = False
-- isLegalMoveFoundation (Card x s1) ((Card y s2):_) = x == succ y && s1 == s2
-- isLegalMoveFoundation _ _ = False


move :: Board -> Int -> Int -> Board
move (Board stackList) index1 index2 =
     let fromStack = stackList !! index1
         toStack = stackList !! index2
         fromType = stackType fromStack
         toType = stackType toStack
         (e:rem) = cards fromStack
         toCards = cards toStack
         resultFrom = Stack (e:toCards) fromType
         resultTo = Stack rem toType
     in Board (toList(update index2 resultTo (update index1 resultFrom (fromList stackList))))
     -- in (Board (toList (fromList[])))


generateMoves :: Board -> [Board]
generateMoves board = [move board x y | x <-[0 .. 11] , y <- [0 .. 11], x /= y]


