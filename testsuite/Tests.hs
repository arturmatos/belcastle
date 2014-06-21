module Main where

import BeleagueredCastle
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit
import Data.List

main :: IO ()
main = defaultMain tests

tests = 
  [testGroup "group1" 
    [testCase "show card" test_1,
     testCase "legalmove" test_2,
     testCase "alreadysolved" test_if_already_solved_return_empty_list,
     testCase "onemove1" test_if_one_move_to_solve_report_that_move ,
     testCase "onemove2" test_if_one_move_to_solve_from_stack1_report_that_move,
     testCase "strsToCards" testConvertCharsToCard,
     testCase "strAsBoard" testConvertFromStringToBoard_noRowsPresent
    ]
  ]


test_1 = assertEqual "" "A\x2661" (show (Card RA Hearts))

test_2 = assertEqual "cannot move ace from foundation to row" 
  False (isLegalMove [Card RA Hearts] [Card R2 Clubs] Row)


test_if_already_solved_return_empty_list = 
  let b = strAsBoard ("KH QH JH 10H 9H 8H 7H 6H 5H 4H 3H 2H AH\n" ++
                     "KD QD JD 10D 9D 8D 7D 6D 5D 4D 3D 2D AD\n" ++
                     "KC QC JC 10C 9C 8C 7C 6C 5C 4C 3C 2C AC\n" ++
                     "KS QS JS 10S 9S 8S 7S 6S 5S 4S 3S 2S AS\n")
  in assertEqual "" (solve b) (Just [])

test_if_one_move_to_solve_report_that_move =
  let b = strAsBoard ("QH JH 10H 9H 8H 7H 6H 5H 4H 3H 2H AH\n" ++
                     "KD QD JD 10D 9D 8D 7D 6D 5D 4D 3D 2D AD\n" ++
                     "KC QC JC 10C 9C 8C 7C 6C 5C 4C 3C 2C AC\n" ++
                     "KS QS JS 10S 9S 8S 7S 6S 5S 4S 3S 2S AS\n" ++
                     "KH\n"
                     )
  in assertEqual "" (solve b) (Just [(4, 0)])
  
test_if_one_move_to_solve_from_stack1_report_that_move =
  let b = strAsBoard ("QH JH 10H 9H 8H 7H 6H 5H 4H 3H 2H AH\n" ++
                     "KD QD JD 10D 9D 8D 7D 6D 5D 4D 3D 2D AD\n" ++
                     "KC QC JC 10C 9C 8C 7C 6C 5C 4C 3C 2C AC\n" ++
                     "KS QS JS 10S 9S 8S 7S 6S 5S 4S 3S 2S AS\n" ++
                     "\n" ++
                     "KH\n"
                     )
  in assertEqual "" (solve b) (Just [(5, 0)])

testConvertCharsToCard = 
  assertEqual "" [Card r s | s <- [Hearts, Diamonds, Spades, Clubs], 
                             r <- [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]]
                 (map strToCard ["AH", "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "10H", "JH", "QH", "KH",
                  "AD", "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "10D", "JD", "QD", "KD",
                  "AS", "2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "10S", "JS", "QS", "KS",
                  "AC", "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "10C", "JC", "QC", "KC"
                 ])

testConvertFromStringToBoard_noRowsPresent = 
  let hearts = Data.List.reverse [Card r Hearts | r <- [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]]
      diamonds = Data.List.reverse [Card r Diamonds | r <- [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]]
      clubs = Data.List.reverse [Card r Clubs | r <- [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]]
      spades = Data.List.reverse [Card r Spades | r <- [RA, R2, R3, R4, R5, R6, R7, R8, R9, R10, RJ, RQ, RK]]
  in assertEqual "" (createBoard [hearts, diamonds, clubs, spades,
                                  [], [], [], [], [], [], [], [] ])
                    (strAsBoard ("KH QH JH 10H 9H 8H 7H 6H 5H 4H 3H 2H AH\n" ++
                                 "KD QD JD 10D 9D 8D 7D 6D 5D 4D 3D 2D AD\n" ++
                                 "KC QC JC 10C 9C 8C 7C 6C 5C 4C 3C 2C AC\n" ++
                                 "KS QS JS 10S 9S 8S 7S 6S 5S 4S 3S 2S AS\n"))

-- TODO quickcheck invariant for move function: number of cards in board must be 52



-- test2 = TestCase (do (x,y) <- partA 3
                     -- assertEqual "for the first result of partA," 5 x
                     -- b <- partB y
                     -- assertBool ("(partB " ++ show y ++ ") failed") b)
