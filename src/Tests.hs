import Test.HUnit
import BeleagueredCastle

test1 = TestCase (assertEqual "" "A\x2661" (show (Card RA Hearts)))



-- test2 = TestCase (do (x,y) <- partA 3
                     -- assertEqual "for the first result of partA," 5 x
                     -- b <- partB y
                     -- assertBool ("(partB " ++ show y ++ ") failed") b)
