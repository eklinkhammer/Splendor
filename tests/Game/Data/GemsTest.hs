module Game.Data.GemsTest (
  gemsTests
  ) where

import qualified Data.Map as Map
import Test.HUnit 
import Game.Data.Gems

gemsTests :: Test
gemsTests = TestLabel "Gems" $ TestList [
  TestLabel "Num" numTests,
  TestLabel "GemHolder" gemHolderTests
  ]

numTests :: Test
numTests = TestList [
  TestLabel "Add" addTest,
  TestLabel "Sub" subtractTest,
  TestLabel "FromInteger" fromIntegerTest
  ]

addTest :: Test
addTest = TestCase (
  assertEqual
  "should return 3" 3 ((+) (fromInteger 2 :: Gems) (fromInteger 1 :: Gems) Map.! (Gem Blue))
  )

subtractTest :: Test
subtractTest = TestCase (
  assertEqual
  "should return 3" 3 ((-) (fromInteger 4 :: Gems) (fromInteger 1 :: Gems) Map.! (Gem Blue))
  )

fromIntegerTest :: Test
fromIntegerTest = test [
  "wild count" ~: "is default (5)" ~: 5 ~=? (fromInteger 3 :: Gems) Map.! (Wild Gold),
  "red count" ~: "is integer value (3)" ~: 3 ~=? (fromInteger 3 :: Gems) Map.! (Gem Red)
  ]

gemHolderTests :: Test
gemHolderTests = TestList [
  TestLabel "gems returns self" gemHolderGemsTest
  ]

gemHolderGemsTest :: Test
gemHolderGemsTest = TestCase (
  assertEqual
  "GemHolder gems are gems"
  (fromInteger 3 :: Gems)
  (gems (fromInteger 3 :: Gems))
  )

