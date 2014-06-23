module BeleagueredCastle (
  Card(Card),
  Suit(..),
  Rank(..),
  StackType(..),
  show,
  isLegalStackMove,
  solve,
  createBoard,
  readBoard,
  readCard
 ) where

import Data.Sequence (fromList, update)
import Data.Foldable
import System.IO
import Data.List
import Data.String


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

data Board = Board [[Card]] deriving (Eq)
instance Show Board where
  show = showBoard

showBoard :: Board -> String
showBoard (Board list) = (intercalate "\n" (map show list))

getStack :: Board -> Int -> [Card]
getStack (Board stackList) index = stackList !! index

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s


-- succeedsInSuit a b means a succeeds b and both cards are of the same suit
succeedsInSuit :: Card -> Card -> Bool
succeedsInSuit _ (Card RK _) = False
succeedsInSuit (Card r1 s1) (Card r2 s2) = (succ r2) == r1 && s1 == s2

-- precedes a b means a precedes b regardless of suit
precedes :: Card -> Card -> Bool
precedes _ (Card RA _) = False
precedes (Card r1 _) (Card r2 _) = (pred r2) == r1

-- isLegalStackMove cards1 cards2 t : is moving the top card from 
-- cards1 to cards2 legal, where cards2 is a stack of type t?
isLegalStackMove :: [Card] -> [Card] -> StackType -> Bool
isLegalStackMove [] _ _ = False  -- consider this clause carefully
isLegalStackMove _ [] _ = False   -- consider this clause carefully, affects termination conditions
isLegalStackMove (topCard:_) (topCard2:_) Foundation = succeedsInSuit topCard topCard2
isLegalStackMove (topCard:_) (topCard2:_) Row = precedes topCard topCard2 && rank topCard /= RA

isLegalMove :: Board -> (Int, Int) -> Bool
isLegalMove board (index1, index2) = 
  isLegalStackMove (getStack board index1) (getStack board index2) (stackType index2)

stackType :: Int -> StackType
stackType i 
  | i < 4     = Foundation
  | otherwise = Row

toSeq = Data.Sequence.fromList

move :: Board -> (Int, Int) -> Board
move (Board stackList) (index1, index2) =
    let (movedCard:remainingSrc) = stackList !! index1
        destStack = stackList !! index2
        resultingTo = (movedCard:destStack)
    in Board (toList(update index2 resultingTo (update index1 remainingSrc (toSeq stackList))))


generateMoves :: Board -> [Board]
generateMoves board =
 let moveIndexes = [(x, y) | x <- [0 .. 11] , y <- [0 .. 11], x /= y]
     possibleMoves = Data.List.filter (isLegalMove board) moveIndexes
 in map (move board) possibleMoves


startingBoard :: [Card] -> Board
startingBoard cards =
    let cardsWithoutAces = [c | c <- cards, rank c /= RA]
        rows = tail (map fst (takeWhile (/= ([], [])) (iterate (\ x -> Data.List.splitAt 6 (snd x)) ([], cardsWithoutAces))))
        foundations = [[Card RA suit] | suit <- [Hearts ..]]
    in createBoard (foundations ++ rows)


-- TODO check there are 12 stacks totalling 52 cards
createBoard :: [[Card]] -> Board
createBoard cards = Board cards


solve :: Board -> Maybe [(Int, Int)]
solve _ = Just []


---------------------------------------------------------
-- Functions to read Boards and Cards from Strings-------
---------------------------------------------------------

readBoard :: String -> Board
readBoard str = 
  let l = lines str
      parsedStacks = (map parseStack l)
      stacks = reverse (ensure 12 [] (reverse parsedStacks))
  in createBoard stacks

ensure n val list 
  | n <= (length list) = list
  | n >  (length list) = ensure n val (val:list)



parseStack :: String -> [Card]
parseStack line = 
  let strlist = words line
  in map readCard strlist

readCard :: String -> Card
readCard ('2':rest) = Card R2 (readSuit rest)
readCard ('3':rest) = Card R3 (readSuit rest)
readCard ('4':rest) = Card R4 (readSuit rest)
readCard ('5':rest) = Card R5 (readSuit rest)
readCard ('6':rest) = Card R6 (readSuit rest)
readCard ('7':rest) = Card R7 (readSuit rest)
readCard ('8':rest) = Card R8 (readSuit rest)
readCard ('9':rest) = Card R9 (readSuit rest)
readCard ('1':'0':rest) = Card R10 (readSuit rest)
readCard ('A':rest) = Card RA (readSuit rest)
readCard ('J':rest) = Card RJ (readSuit rest)
readCard ('Q':rest) = Card RQ (readSuit rest)
readCard ('K':rest) = Card RK (readSuit rest)

readSuit :: String -> Suit
readSuit ('H':[]) = Hearts
readSuit ('D':[]) = Diamonds
readSuit ('S':[]) = Spades
readSuit ('C':[]) = Clubs

