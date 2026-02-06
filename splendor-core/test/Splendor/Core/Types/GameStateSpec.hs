module Splendor.Core.Types.GameStateSpec (spec) where

import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.TestHelpers

spec :: Spec
spec = do
  describe "currentPlayer" $ do
    it "returns Just for valid index 0" $ do
      let p1 = emptyPlayer "p1"
          p2 = emptyPlayer "p2"
          board = mkTestBoard twoPlayerBank []
          gs = mkGameState [p1, p2] board
      currentPlayer gs `shouldBe` Just p1

    it "returns Nothing for empty player list" $ do
      let board = mkTestBoard twoPlayerBank []
          gs = mkGameState [] board
      currentPlayer gs `shouldBe` Nothing

    it "returns Nothing for negative index" $ do
      let p1 = emptyPlayer "p1"
          board = mkTestBoard twoPlayerBank []
          gs = (mkGameState [p1] board) { gsCurrentPlayer = -1 }
      currentPlayer gs `shouldBe` Nothing

    it "returns Nothing for index past end of list" $ do
      let p1 = emptyPlayer "p1"
          board = mkTestBoard twoPlayerBank []
          gs = (mkGameState [p1] board) { gsCurrentPlayer = 5 }
      currentPlayer gs `shouldBe` Nothing

    it "returns correct player for non-zero valid index" $ do
      let p1 = emptyPlayer "p1"
          p2 = emptyPlayer "p2"
          p3 = emptyPlayer "p3"
          board = mkTestBoard twoPlayerBank []
          gs = (mkGameState [p1, p2, p3] board) { gsCurrentPlayer = 2 }
      currentPlayer gs `shouldBe` Just p3
