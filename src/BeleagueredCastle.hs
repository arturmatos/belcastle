module BeleagueredCastle (
  Card(Card),
  Suit(..),
  Rank(..),
  Stack(..),
  StackType(..),
  show,
  isLegalMove,
  solve,
  strAsBoard
 ) where

import Data.Sequence
import Data.Foldable
import System.IO
import Data.List


data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Enum)
instance Show Suit where
  show Hearts   = "\x2661"
  show Diamonds = "\x2662"
  show Clubs    = "\x2663"
  show Spades   = "\x2660"


data Rank = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK deriving (Enum, Eq)
instance Show Rank where
  show RA = "A"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = "10"
  show RJ = "J"
  show RQ = "Q"
  show RK = "K"

type Move = (Int, Int)

allRanks = [RA ..]
allSuits = [Hearts ..]

data Card = Card Rank Suit deriving (Eq)
instance Show Card where
  show (Card r s) = (show r) ++ (show s)


allCards = [Card r s | r <- allRanks, s <- allSuits]

data StackType = Foundation | Row deriving (Show, Eq)
data Stack = Stack [Card] StackType  
instance Show Stack where
  show (Stack (x:xs) Foundation) = "[ " ++ (show x) ++ " ]"
  show (Stack cardList Row) = "[ " ++ (intercalate "  " (map show cardList)) ++ " ]"

data Board = Board [Stack] 
instance Show Board where
  show = showBoard

showBoard :: Board -> String
showBoard (Board list) = 
  let foundations = Data.List.filter (\x -> stackType x == Foundation) list  
      rows        = Data.List.filter (\x -> stackType x == Row) list  
  in (show foundations) ++ "\n" ++ (intercalate "\n" (map show rows))




getStack :: Board -> Int -> Stack
getStack (Board stackList) index = stackList !! index

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

cards :: Stack -> [Card]
cards (Stack cards stackType) = cards

stackType :: Stack -> StackType
stackType (Stack cards stackType) = stackType

succeedsInSuit :: Card -> Card -> Bool
succeedsInSuit _ (Card RK _) = False
succeedsInSuit (Card r1 s1) (Card r2 s2) = (succ r2) == r1 && s1 == s2

precedes :: Card -> Card -> Bool
precedes _ (Card RA _) = False
precedes (Card r1 _) (Card r2 _) = (pred r2) == r1

isLegalMove :: Stack -> Stack -> Bool
isLegalMove (Stack (topCard:_) _) (Stack (topCard2:_) Foundation) = succeedsInSuit topCard topCard2
isLegalMove (Stack (topCard:_) _) (Stack (topCard2:_) Row) = precedes topCard topCard2 && rank topCard /= RA

isLegalMoveBoard :: Board -> (Int, Int) -> Bool
isLegalMoveBoard board (index1, index2) = isLegalMove (getStack board index1) (getStack board index2)

move :: Board -> (Int, Int) -> Board
move (Board stackList) (index1, index2) =
    let fromStack = stackList !! index1
        toStack = stackList !! index2
        fromType = stackType fromStack
        toType = stackType toStack
        (e:rem) = cards fromStack
        toCards = cards toStack
        resultFrom = Stack rem fromType
        resultTo = Stack (e:toCards) toType
    in Board (toList(update index2 resultTo (update index1 resultFrom (fromList stackList))))


generateMoves :: Board -> [Board]
generateMoves board =
 let moveIndexes = [(x, y) | x <- [0 .. 11] , y <- [0 .. 11], x /= y]
     possibleMoves = Data.List.filter (isLegalMoveBoard board) moveIndexes
 in map (move board) possibleMoves



createBoard :: [Card] -> Board
createBoard cards =
    let cardsWithoutAces = [c | c <- cards, rank c /= RA]
        rows = tail (map fst (takeWhile (/= ([], [])) (iterate (\ x -> Data.List.splitAt 6 (snd x)) ([], cardsWithoutAces))))
        foundations = [[Card RA suit] | suit <- [Hearts ..]]
    in asBoard foundations rows

asBoard f r = Board ((asFoundations f) ++ (asRows r))

asFoundations [w,x,y,z] = [Stack w Foundation,
                          Stack x Foundation,
                          Stack y Foundation,
                          Stack z Foundation]

asRows [] = []
asRows (x:xs) = (Stack x Row) : (asRows xs)


solve :: Board -> Maybe [(Int, Int)]
solve _ = Just []

strAsBoard :: String -> Board
strAsBoard str = createBoard allCards
