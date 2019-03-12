module Tests.Tests where

import Test.HUnit hiding (State)

-- HUnit stuff

{-
test1 :: Test
test1 = "1 + 2" ~: 3 ~=? (1 + 2)

tests :: Test
tests = TestList [TestLabel "t1" test1]

testConcat :: Test -> Test -> Test
testConcat (TestList ts1) (TestList ts2) = TestList (ts1 ++ ts2)

concatTests :: [Test] -> Test
concatTests = foldr testConcat (TestList []) 
-}
