module Main where

import BeleagueredCastle
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testCase "sort7" test_1,
                testCase "sort8" test_2
            ]
    ]


test_1 = assertEqual "" "A\x2661" (show (Card RA Hearts))

test_2 = assertEqual "cannot move ace from foundation to row" 
  False (isLegalMove (Stack [Card RA Hearts] Foundation) (Stack [Card R2 Clubs] Row))

-- test2 = TestCase (assertEqual "" 
  -- "[[ A♡ ],[ A♢ ],[ A♣ ],[ A♠ ]]\n" ++
  -- "[  ]\n" ++
  -- "[ 3♣  3♠  4♡  4♢  4♣  4♠ ]\n" ++
  -- "[ 5♡  5♢  5♣  5♠  6♡  6♢ ]\n" ++
  -- "[ 6♣  6♠  7♡  7♢  7♣  7♠ ]\n" ++
  -- "[ 8♡  8♢  8♣  8♠  9♡  9♢ ]\n" ++
  -- "[ 9♣  9♠  10♡  10♢  10♣  10♠ ]\n" ++
  -- "[ J♡  J♢  J♣  J♠  Q♡  Q♢ ]\n" ++
  -- "[ Q♣  Q♠  K♡  K♢  K♣  K♠ ]\n",
  -- show (move board (0,4))

-- quickcheck invariant for move function: number of cards in board must be 52



-- test2 = TestCase (do (x,y) <- partA 3
                     -- assertEqual "for the first result of partA," 5 x
                     -- b <- partB y
                     -- assertBool ("(partB " ++ show y ++ ") failed") b)
