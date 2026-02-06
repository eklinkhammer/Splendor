{-# OPTIONS_GHC -Wno-x-partial #-}
module Splendor.Core.SetupSpec (spec) where

import System.Random (mkStdGen)
import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Setup (initGameState)

spec :: Spec
spec = do
  describe "Setup" $ do
    describe "initGameState" $ do
      let gs2 = initGameState (mkStdGen 42) "test" 2 ["Alice", "Bob"]
          gs3 = initGameState (mkStdGen 42) "test" 3 ["A", "B", "C"]
          gs4 = initGameState (mkStdGen 42) "test" 4 ["A", "B", "C", "D"]

      it "2-player bank: 4 per color, 5 gold" $ do
        let bank = boardBank (gsBoard gs2)
        gemCount bank (GemToken Diamond) `shouldBe` 4
        gemCount bank (GemToken Sapphire) `shouldBe` 4
        gemCount bank (GemToken Emerald) `shouldBe` 4
        gemCount bank (GemToken Ruby) `shouldBe` 4
        gemCount bank (GemToken Onyx) `shouldBe` 4
        gemCount bank GoldToken `shouldBe` 5

      it "3-player bank: 5 per color, 5 gold" $ do
        let bank = boardBank (gsBoard gs3)
        gemCount bank (GemToken Diamond) `shouldBe` 5
        gemCount bank GoldToken `shouldBe` 5

      it "4-player bank: 7 per color, 5 gold" $ do
        let bank = boardBank (gsBoard gs4)
        gemCount bank (GemToken Diamond) `shouldBe` 7
        gemCount bank GoldToken `shouldBe` 5

      it "each tier display has 4 cards" $ do
        let board = gsBoard gs2
        length (tierDisplay (boardTier1 board)) `shouldBe` 4
        length (tierDisplay (boardTier2 board)) `shouldBe` 4
        length (tierDisplay (boardTier3 board)) `shouldBe` 4

      it "nobles = playerCount + 1" $ do
        length (boardNobles (gsBoard gs2)) `shouldBe` 3
        length (boardNobles (gsBoard gs3)) `shouldBe` 4
        length (boardNobles (gsBoard gs4)) `shouldBe` 5

      it "players start empty (no tokens/cards/nobles)" $ do
        let p = gsPlayers gs2 !! 0
        playerTokenCount p `shouldBe` 0
        playerPurchased p `shouldBe` []
        playerReserved p `shouldBe` []
        playerNobles p `shouldBe` []

      it "total cards across decks + displays = 90" $ do
        let board = gsBoard gs2
            total = length (tierDeck (boardTier1 board)) + length (tierDisplay (boardTier1 board))
                  + length (tierDeck (boardTier2 board)) + length (tierDisplay (boardTier2 board))
                  + length (tierDeck (boardTier3 board)) + length (tierDisplay (boardTier3 board))
        total `shouldBe` 90

      it "different seeds produce different orderings" $ do
        let gs_a = initGameState (mkStdGen 1) "test" 2 ["A", "B"]
            gs_b = initGameState (mkStdGen 99999) "test" 2 ["A", "B"]
            display_a = tierDisplay (boardTier1 (gsBoard gs_a))
            display_b = tierDisplay (boardTier1 (gsBoard gs_b))
        display_a `shouldNotBe` display_b
