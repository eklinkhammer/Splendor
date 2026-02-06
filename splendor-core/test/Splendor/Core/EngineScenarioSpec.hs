module Splendor.Core.EngineScenarioSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Engine
import Splendor.Core.Rules.ActionValidation (legalActions)
import Splendor.Core.TestHelpers

spec :: Spec
spec = describe "Engine scenarios" $ do

  -- ========== Block 1: Deck depletion edge cases ==========
  describe "deck depletion edge cases" $ do

    it "buy last display card with empty deck" $ do
      let card1 = mkCard "c1" Tier1 [(Ruby, 1)] Diamond 0
          tokens = mkGems [(GemToken Ruby, 2)]
          p1 = playerWithTokens "p1" tokens
          p2 = emptyPlayer "p2"
          -- Tier1: empty deck, 1 display card
          board = Board
            { boardTier1 = mkTierRow [] [card1]
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
          payment = mkGems [(GemToken Ruby, 1)]
      case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
        Advanced gs' -> do
          let p = gsPlayers gs' !! 0
          length (playerPurchased p) `shouldBe` 1
          tierDisplay (boardTier1 (gsBoard gs')) `shouldBe` []
          tierDeck (boardTier1 (gsBoard gs')) `shouldBe` []
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    it "reserve from deck with exactly 1 card" $ do
      let deckCard = mkCard "d1" Tier2 [] Ruby 0
          p1 = emptyPlayer "p1"
          p2 = emptyPlayer "p2"
          board = Board
            { boardTier1 = mkTierRow [] []
            , boardTier2 = mkTierRow [deckCard] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
      case applyAction gs "p1" (ReserveCard (FromTopOfDeck Tier2)) of
        Advanced gs' -> do
          let p = gsPlayers gs' !! 0
          length (playerReserved p) `shouldBe` 1
          tierDeck (boardTier2 (gsBoard gs')) `shouldBe` []
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    it "all cards in a tier gone: no Tier1 actions generated" $ do
      let card2 = mkCard "c2" Tier2 [(Ruby, 1)] Diamond 0
          p1 = emptyPlayer "p1"
          p2 = emptyPlayer "p2"
          -- Tier1 completely empty, Tier2 has a card
          board = Board
            { boardTier1 = mkTierRow [] []
            , boardTier2 = mkTierRow [] [card2]
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
          actions = legalActions gs
          -- No Tier1 references should exist
          hasTier1Ref = any isTier1Action actions
      hasTier1Ref `shouldBe` False

  -- ========== Block 2: Bank depletion scenarios ==========
  describe "bank depletion scenarios" $ do

    it "only 2 gem colors in bank: TakeDifferent limited to 2" $ do
      let bank = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1), (GoldToken, 5)]
          p1 = emptyPlayer "p1"
          p2 = emptyPlayer "p2"
          board = mkTestBoard bank []
          gs = mkGameState [p1, p2] board
          actions = legalActions gs
          gemTakes = [ gt | TakeGems gt <- actions ]
          -- Should only generate TakeDifferent [Diamond, Ruby] or [Ruby, Diamond]
          diffTakes = [ cs | TakeDifferent cs <- gemTakes ]
      -- All TakeDifferent should have exactly 2 colors (the 2 available)
      all (\cs -> length cs == 2) diffTakes `shouldBe` True
      -- No TakeTwoSame should be available (need >= 4 for that)
      any (\case TakeTwoSame _ -> True; _ -> False) gemTakes `shouldBe` False

    it "no gem colors in bank (gold only): no TakeGems actions" $ do
      let bank = mkGems [(GoldToken, 5)]
          card1 = mkCard "c1" Tier1 [] Diamond 0
          p1 = emptyPlayer "p1"
          p2 = emptyPlayer "p2"
          board = mkTestBoard bank [card1]
          gs = mkGameState [p1, p2] board
          actions = legalActions gs
          hasTakeGems = any (\case TakeGems _ -> True; _ -> False) actions
          hasReserve = any (\case ReserveCard _ -> True; _ -> False) actions
      hasTakeGems `shouldBe` False
      hasReserve `shouldBe` True

  -- ========== Block 3: Bonus-based payment and free cards ==========
  describe "bonus-based payment and free cards" $ do

    it "full bonus coverage: zero payment" $ do
      let card = mkCard "target" Tier1 [(Ruby, 3), (Diamond, 2)] Emerald 1
          -- 3 Ruby bonuses + 2 Diamond bonuses
          rubyCards = [ mkCard ("r" <> T.pack (show i)) Tier1 [] Ruby 0 | i <- [1..3 :: Int] ]
          diamondCards = [ mkCard ("d" <> T.pack (show i)) Tier1 [] Diamond 0 | i <- [1..2 :: Int] ]
          p1 = playerWithCards "p1" (rubyCards ++ diamondCards)
          p2 = emptyPlayer "p2"
          board = Board
            { boardTier1 = mkTierRow [] [card]
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
      case applyAction gs "p1" (BuyCard (FromDisplay "target") emptyGems) of
        Advanced gs' -> do
          let p = gsPlayers gs' !! 0
          length (playerPurchased p) `shouldBe` 6  -- 5 existing + 1 new
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    it "partial bonus + gold wildcard" $ do
      -- Card costs Ruby(4)+Emerald(2)
      -- Player has 2 Ruby bonuses, so effective cost is Ruby(2)+Emerald(2)
      -- Player tokens: Ruby(1)+Emerald(2)+Gold(1) → pay Ruby(1)+Emerald(2)+Gold(1)
      let card = mkCard "target" Tier1 [(Ruby, 4), (Emerald, 2)] Diamond 0
          rubyCards = [ mkCard ("r" <> T.pack (show i)) Tier1 [] Ruby 0 | i <- [1..2 :: Int] ]
          tokens = mkGems [(GemToken Ruby, 1), (GemToken Emerald, 2), (GoldToken, 1)]
          p1 = (playerWithTokens "p1" tokens) { playerPurchased = rubyCards }
          p2 = emptyPlayer "p2"
          board = Board
            { boardTier1 = mkTierRow [] [card]
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
          payment = mkGems [(GemToken Ruby, 1), (GemToken Emerald, 2), (GoldToken, 1)]
      case applyAction gs "p1" (BuyCard (FromDisplay "target") payment) of
        Advanced gs' -> do
          let p = gsPlayers gs' !! 0
          length (playerPurchased p) `shouldBe` 3  -- 2 existing + 1 new
          totalGems (playerTokens p) `shouldBe` 0
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    it "bonuses exceed cost: zero payment still works" $ do
      let card = mkCard "target" Tier1 [(Diamond, 2)] Emerald 1
          -- 5 Diamond bonuses (exceeds the 2 needed)
          diamondCards = [ mkCard ("d" <> T.pack (show i)) Tier1 [] Diamond 0 | i <- [1..5 :: Int] ]
          p1 = playerWithCards "p1" diamondCards
          p2 = emptyPlayer "p2"
          board = Board
            { boardTier1 = mkTierRow [] [card]
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
      case applyAction gs "p1" (BuyCard (FromDisplay "target") emptyGems) of
        Advanced gs' -> do
          let p = gsPlayers gs' !! 0
          length (playerPurchased p) `shouldBe` 6
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

  -- ========== Block 4: FinalRound wrap-around ==========
  describe "FinalRound wrap-around" $ do

    it "3-player FinalRound: wraps to P0 triggers GameOver" $ do
      -- P0 has 15 prestige (triggered FinalRound), P1 and P2 take gems, then wrap → GameOver
      let existingCards = [ mkCard "big" Tier1 [] Diamond 15 ]
          p1 = playerWithCards "p1" existingCards
          p2 = emptyPlayer "p2"
          p3 = emptyPlayer "p3"
          bank = mkGems $ [(GemToken c, 5) | c <- allGemColors] ++ [(GoldToken, 5)]
          board = mkTestBoard bank []
          gs = (mkGameState [p1, p2, p3] board)
                 { gsPhase = FinalRound
                 , gsCurrentPlayer = 1
                 }
      -- P2 (index 1) takes gems
      case applyAction gs "p2" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald])) of
        Advanced gs' -> do
          gsCurrentPlayer gs' `shouldBe` 2
          gsPhase gs' `shouldBe` FinalRound
          -- P3 (index 2) takes gems → wrap to 0 → GameOver
          case applyAction gs' "p3" (TakeGems (TakeDifferent [Sapphire, Onyx, Ruby])) of
            GameOver _ result -> do
              winnerId result `shouldBe` "p1"
              finalPrestige result `shouldBe` 15
            other -> expectationFailure $ "Expected GameOver, got: " ++ show other
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    it "non-zero player triggers FinalRound: wrap to P0 ends game" $ do
      -- P1 (index 1) has 12 prestige, buys 3-prestige card → 15 → FinalRound
      -- P2 takes gems, wrap to P0 → GameOver
      let existingCards = [ mkCard "big" Tier1 [] Diamond 12 ]
          bigCard = mkCard "win" Tier2 [(Ruby, 1)] Emerald 3
          tokens = mkGems [(GemToken Ruby, 2)]
          p1 = emptyPlayer "p1"
          p2 = (playerWithTokens "p2" tokens) { playerPurchased = existingCards }
          p3 = emptyPlayer "p3"
          bank = mkGems $ [(GemToken c, 5) | c <- allGemColors] ++ [(GoldToken, 5)]
          board = Board
            { boardTier1 = mkTierRow [] []
            , boardTier2 = mkTierRow [] [bigCard]
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = bank
            }
          gs = (mkGameState [p1, p2, p3] board)
                 { gsCurrentPlayer = 1 }
          payment = mkGems [(GemToken Ruby, 1)]
      case applyAction gs "p2" (BuyCard (FromDisplay "win") payment) of
        Advanced gs' -> do
          gsPhase gs' `shouldBe` FinalRound
          gsCurrentPlayer gs' `shouldBe` 2
          -- P3 takes gems → wrap to P0 → GameOver
          case applyAction gs' "p3" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald])) of
            GameOver _ result -> do
              winnerId result `shouldBe` "p2"
              finalPrestige result `shouldBe` 15
            other -> expectationFailure $ "Expected GameOver after wrap, got: " ++ show other
        other -> expectationFailure $ "Expected Advanced with FinalRound, got: " ++ show other

    it "noble-triggered win during FinalRound" $ do
      -- Already in FinalRound. P1 (index 1) buys a Diamond card, meets noble req, auto-noble → 15 prestige
      let noble = mkNoble "n1" [(Diamond, 3)]
          existingCards = [ mkCard ("d" <> T.pack (show i)) Tier1 [] Diamond 0 | i <- [1..2 :: Int] ]
          -- Existing prestige: 0 from cards. Noble gives 3. Need 12 more from existing.
          prestigeCard = mkCard "prestige" Tier1 [] Emerald 12
          newDiamondCard = mkCard "newD" Tier1 [(Ruby, 1)] Diamond 0
          tokens = mkGems [(GemToken Ruby, 2)]
          p1 = emptyPlayer "p1"
          p2 = (playerWithTokens "p2" tokens)
                 { playerPurchased = existingCards ++ [prestigeCard] }
          p3 = emptyPlayer "p3"
          bank = mkGems $ [(GemToken c, 5) | c <- allGemColors] ++ [(GoldToken, 5)]
          board = Board
            { boardTier1 = mkTierRow [] [newDiamondCard]
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = [noble]
            , boardBank = bank
            }
          gs = (mkGameState [p1, p2, p3] board)
                 { gsPhase = FinalRound
                 , gsCurrentPlayer = 1
                 }
          payment = mkGems [(GemToken Ruby, 1)]
      -- P2 buys Diamond card → 3 Diamond bonuses → noble auto-visits → 12+3 = 15 prestige
      case applyAction gs "p2" (BuyCard (FromDisplay "newD") payment) of
        Advanced gs' -> do
          -- P2 should have the noble
          let p = gsPlayers gs' !! 1
          length (playerNobles p) `shouldBe` 1
          -- Next turn wraps → GameOver
          case applyAction gs' "p3" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald])) of
            GameOver _ result -> do
              winnerId result `shouldBe` "p2"
              finalPrestige result `shouldBe` 15
            other -> expectationFailure $ "Expected GameOver after wrap, got: " ++ show other
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

  -- ========== Block 5: Multi-step strategic sequences ==========
  describe "multi-step strategic sequences" $ do

    it "reserve then buy from reserve with gold" $ do
      let card = mkCard "c1" Tier1 [(Ruby, 1)] Diamond 0
          p1 = emptyPlayer "p1"
          p2 = emptyPlayer "p2"
          board = Board
            { boardTier1 = mkTierRow [] [card]
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
      -- P1 reserves card (gets gold)
      case applyAction gs "p1" (ReserveCard (FromDisplay "c1")) of
        Advanced gs' -> do
          let p = gsPlayers gs' !! 0
          length (playerReserved p) `shouldBe` 1
          gemCount (playerTokens p) GoldToken `shouldBe` 1
          -- P2 takes gems
          case applyAction gs' "p2" (TakeGems (TakeDifferent [Ruby, Diamond, Emerald])) of
            Advanced gs'' -> do
              -- P1 buys from reserve paying with gold
              let goldPayment = mkGems [(GoldToken, 1)]
              case applyAction gs'' "p1" (BuyCard (FromReserve "c1") goldPayment) of
                Advanced gs''' -> do
                  let p1' = gsPlayers gs''' !! 0
                  length (playerPurchased p1') `shouldBe` 1
                  length (playerReserved p1') `shouldBe` 0
                  gemCount (playerTokens p1') GoldToken `shouldBe` 0
                other -> expectationFailure $ "Expected Advanced on buy, got: " ++ show other
            other -> expectationFailure $ "Expected Advanced on P2 turn, got: " ++ show other
        other -> expectationFailure $ "Expected Advanced on reserve, got: " ++ show other

    it "reserve triggers gem return when at 10 tokens" $ do
      let card = mkCard "c1" Tier1 [] Diamond 0
          tokens = mkGems [(GemToken Ruby, 5), (GemToken Diamond, 5)]
          p1 = playerWithTokens "p1" tokens
          p2 = emptyPlayer "p2"
          board = Board
            { boardTier1 = mkTierRow [] [card]
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] []
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
      -- Reserve gives gold → 11 tokens → NeedGemReturn 1
      case applyAction gs "p1" (ReserveCard (FromDisplay "c1")) of
        NeedGemReturn gs' excess -> do
          excess `shouldBe` 1
          -- Set the turn phase as the server would
          let gs'r = gs' { gsTurnPhase = MustReturnGems excess }
          -- Return 1 Ruby
          let ret = singleGem (GemToken Ruby) 1
          case applyGemReturn gs'r "p1" ret of
            Advanced gs'' -> do
              let p = gsPlayers gs'' !! 0
              totalGems (playerTokens p) `shouldBe` 10
              -- Turn should have advanced to P2
              gsCurrentPlayer gs'' `shouldBe` 1
            other -> expectationFailure $ "Expected Advanced after gem return, got: " ++ show other
        other -> expectationFailure $ "Expected NeedGemReturn, got: " ++ show other

    it "bonus chain: buy expensive Tier3 card with bonuses" $ do
      -- Player has 6 Ruby bonuses + 1 Ruby token. Tier3 card costs Ruby(7). Effective cost: Ruby(1).
      let rubyCards = [ mkCard ("r" <> T.pack (show i)) Tier1 [] Ruby 0 | i <- [1..6 :: Int] ]
          expensiveCard = mkCard "t3" Tier3 [(Ruby, 7)] Diamond 5
          tokens = mkGems [(GemToken Ruby, 1)]
          p1 = (playerWithTokens "p1" tokens) { playerPurchased = rubyCards }
          p2 = emptyPlayer "p2"
          board = Board
            { boardTier1 = mkTierRow [] []
            , boardTier2 = mkTierRow [] []
            , boardTier3 = mkTierRow [] [expensiveCard]
            , boardNobles = []
            , boardBank = twoPlayerBank
            }
          gs = mkGameState [p1, p2] board
          payment = mkGems [(GemToken Ruby, 1)]
      case applyAction gs "p1" (BuyCard (FromDisplay "t3") payment) of
        Advanced gs' -> do
          let p = gsPlayers gs' !! 0
          -- Should have 7 cards total (6 ruby + expensive)
          length (playerPurchased p) `shouldBe` 7
          -- Should have 0 Ruby tokens left
          gemCount (playerTokens p) (GemToken Ruby) `shouldBe` 0
          -- Prestige gained: 5 from the Tier3 card
          playerPrestige p `shouldBe` 5
        GameOver _ _ -> pure ()  -- Also acceptable if it triggers win
        other -> expectationFailure $ "Expected Advanced, got: " ++ show other

-- ========== Helpers ==========

-- | Check if an action references Tier1
isTier1Action :: Action -> Bool
isTier1Action (BuyCard (FromDisplay _) _) = False  -- Can't tell tier from CardId alone
isTier1Action (BuyCard (FromTopOfDeck Tier1) _) = True
isTier1Action (ReserveCard (FromTopOfDeck Tier1)) = True
isTier1Action (ReserveCard (FromDisplay _)) = False  -- Can't tell tier from CardId
isTier1Action _ = False
