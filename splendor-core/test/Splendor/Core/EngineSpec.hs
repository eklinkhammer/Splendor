module Splendor.Core.EngineSpec (spec) where

import Data.Either (isRight)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import System.Random (mkStdGen, uniformR, StdGen)
import Test.Hspec
import Test.QuickCheck
import Splendor.Core.Types
import Splendor.Core.Engine
import Splendor.Core.Setup (initGameState)
import Splendor.Core.Rules.ActionValidation (validateAction, legalActions, legalGemReturns)
import Splendor.Core.TestHelpers

spec :: Spec
spec = do
  describe "Engine" $ do
    -- ========== Existing tests ==========
    describe "initGameState" $ do
      it "creates a valid 2-player game" $ do
        let gs = initGameState (mkStdGen 42) "test" 2 ["Alice", "Bob"]
        length (gsPlayers gs) `shouldBe` 2
        gsCurrentPlayer gs `shouldBe` 0
        gsPhase gs `shouldBe` InProgress
        gsTurnPhase gs `shouldBe` AwaitingAction
        length (tierDisplay (boardTier1 (gsBoard gs))) `shouldBe` 4
        length (tierDisplay (boardTier2 (gsBoard gs))) `shouldBe` 4
        length (tierDisplay (boardTier3 (gsBoard gs))) `shouldBe` 4
        length (boardNobles (gsBoard gs)) `shouldBe` 3  -- 2 players + 1

      it "creates a valid 4-player game" $ do
        let gs = initGameState (mkStdGen 99) "test" 4 ["A", "B", "C", "D"]
        length (gsPlayers gs) `shouldBe` 4
        length (boardNobles (gsBoard gs)) `shouldBe` 5  -- 4 players + 1
        gemCount (boardBank (gsBoard gs)) (GemToken Diamond) `shouldBe` 7

    describe "legalActions" $ do
      it "produces actions for the initial state" $ do
        let gs = initGameState (mkStdGen 42) "test" 2 ["Alice", "Bob"]
            actions = legalActions gs
        length actions `shouldSatisfy` (> 0)

    describe "applyAction" $ do
      it "advances turn after taking gems" $ do
        let gs = initGameState (mkStdGen 42) "test" 2 ["Alice", "Bob"]
            actions = legalActions gs
            gemAction = case [ a | a@(TakeGems _) <- actions ] of
              (a:_) -> a
              []    -> error "No gem-take actions in initial state"
            pid = playerId (fromJust (currentPlayer gs))
            result = applyAction gs pid gemAction
        case result of
          Advanced gs' -> gsCurrentPlayer gs' `shouldBe` 1
          NeedGemReturn _ _ -> pure ()  -- Also valid
          other -> expectationFailure $ "Expected Advanced or NeedGemReturn, got: " ++ show other

    -- ========== New applyAction - TakeGems tests ==========
    describe "applyAction - TakeGems details" $ do
      it "bank decremented, player incremented by correct amounts" $ do
        let gs = initGameState (mkStdGen 42) "test" 2 ["Alice", "Bob"]
            pid = playerId (fromJust (currentPlayer gs))
            action = TakeGems (TakeDifferent [Ruby, Diamond, Emerald])
        case applyAction gs pid action of
          Advanced gs' -> do
            -- Player 1 (now past, index 0) should have 3 gems
            let p0 = gsPlayers gs' !! 0
            gemCount (playerTokens p0) (GemToken Ruby) `shouldBe` 1
            gemCount (playerTokens p0) (GemToken Diamond) `shouldBe` 1
            gemCount (playerTokens p0) (GemToken Emerald) `shouldBe` 1
            -- Bank should be decremented
            gemCount (boardBank (gsBoard gs')) (GemToken Ruby) `shouldBe` 3
            gemCount (boardBank (gsBoard gs')) (GemToken Diamond) `shouldBe` 3
            gemCount (boardBank (gsBoard gs')) (GemToken Emerald) `shouldBe` 3
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "taking to >10 tokens triggers NeedGemReturn" $ do
        let tokens = mkGems [(GemToken Ruby, 4), (GemToken Diamond, 4)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank []
            gs = mkGameState [p1, p2] board
            -- Take 3 more = 11 total, need to return 1
            action = TakeGems (TakeDifferent [Emerald, Sapphire, Onyx])
        case applyAction gs "p1" action of
          NeedGemReturn _ excess -> excess `shouldBe` 1
          other -> expectationFailure $ "Expected NeedGemReturn, got: " ++ show other

      it "taking to exactly 10 tokens: Advanced" $ do
        let tokens = mkGems [(GemToken Ruby, 4), (GemToken Diamond, 3)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank []
            gs = mkGameState [p1, p2] board
            -- Take 3 more = 10 total, no gem return
            action = TakeGems (TakeDifferent [Emerald, Sapphire, Onyx])
        case applyAction gs "p1" action of
          Advanced _ -> pure ()
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    -- ========== New applyAction - BuyCard tests ==========
    describe "applyAction - BuyCard" $ do
      it "card moves to purchased, removed from display" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Diamond 0
            tokens = mkGems [(GemToken Ruby, 3)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] [card1]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 2)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            case playerPurchased p of
              (c:_) -> cardId c `shouldBe` "c1"
              []    -> expectationFailure "Expected at least one purchased card"
            -- Card removed from display
            length (tierDisplay (boardTier1 (gsBoard gs'))) `shouldBe` 0
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "payment tokens return to bank" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Diamond 0
            tokens = mkGems [(GemToken Ruby, 3)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] [card1]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 2)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          Advanced gs' -> do
            -- Player had 3 Ruby, paid 2, should have 1
            let p = gsPlayers gs' !! 0
            gemCount (playerTokens p) (GemToken Ruby) `shouldBe` 1
            -- Bank had 4 Ruby (twoPlayerBank), gets 2 back = 6
            gemCount (boardBank (gsBoard gs')) (GemToken Ruby) `shouldBe` 6
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "display refills from deck after buy" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Diamond 0
            card2 = mkCard "c2" Tier1 [] Sapphire 0
            tokens = mkGems [(GemToken Ruby, 3)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [card2] [card1]  -- deck has card2
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 2)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          Advanced gs' -> do
            -- card2 should have moved from deck to display
            let display = tierDisplay (boardTier1 (gsBoard gs'))
            case display of
              (c:_) -> cardId c `shouldBe` "c2"
              []    -> expectationFailure "Expected display to contain a card"
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "buy from reserve: card removed from reserve" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Diamond 0
            tokens = mkGems [(GemToken Ruby, 3)]
            p1 = (playerWithTokens "p1" tokens) { playerReserved = [card1] }
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 2)]
        case applyAction gs "p1" (BuyCard (FromReserve "c1") payment) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            length (playerReserved p) `shouldBe` 0
            length (playerPurchased p) `shouldBe` 1
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "purchase triggering 15+ prestige: enters FinalRound" $ do
        -- Give player 12 prestige from existing cards, buy a 3-prestige card
        let existingCards = [ mkCard ("e" <> T.pack (show i)) Tier1 [] Diamond
                              (if i == 1 then 12 else 0)
                            | i <- [1..4 :: Int] ]
            bigCard = mkCard "big" Tier2 [(Ruby, 1)] Emerald 3
            tokens = mkGems [(GemToken Ruby, 2)]
            p1 = (playerWithTokens "p1" tokens) { playerPurchased = existingCards }
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] [bigCard]
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1)]
        case applyAction gs "p1" (BuyCard (FromDisplay "big") payment) of
          Advanced gs' -> gsPhase gs' `shouldBe` FinalRound
          GameOver _ _ -> pure ()  -- Also acceptable
          other -> expectationFailure $ "Expected Advanced with FinalRound or GameOver, got: " ++ show other

    -- ========== New applyAction - ReserveCard tests ==========
    describe "applyAction - ReserveCard" $ do
      it "card moves to reserve, display refills" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Diamond 0
            card2 = mkCard "c2" Tier1 [] Sapphire 0
            p1 = emptyPlayer "p1"
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [card2] [card1]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
        case applyAction gs "p1" (ReserveCard (FromDisplay "c1")) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            case playerReserved p of
              (c:_) -> cardId c `shouldBe` "c1"
              []    -> expectationFailure "Expected at least one reserved card"
            -- Display should have refilled with card2
            length (tierDisplay (boardTier1 (gsBoard gs'))) `shouldBe` 1
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "gold awarded when available" $ do
        let p1 = emptyPlayer "p1"
            p2 = emptyPlayer "p2"
            card1 = mkCard "c1" Tier1 [] Diamond 0
            board = Board
              { boardTier1 = mkTierRow [] [card1]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank  -- has 5 gold
              }
            gs = mkGameState [p1, p2] board
        case applyAction gs "p1" (ReserveCard (FromDisplay "c1")) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            gemCount (playerTokens p) GoldToken `shouldBe` 1
            gemCount (boardBank (gsBoard gs')) GoldToken `shouldBe` 4
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "no gold when bank empty" $ do
        let p1 = emptyPlayer "p1"
            p2 = emptyPlayer "p2"
            card1 = mkCard "c1" Tier1 [] Diamond 0
            bank = mkGems [(GemToken Ruby, 4), (GemToken Diamond, 4), (GemToken Emerald, 4),
                           (GemToken Sapphire, 4), (GemToken Onyx, 4)]
            -- No gold in bank
            board = Board
              { boardTier1 = mkTierRow [] [card1]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = bank
              }
            gs = mkGameState [p1, p2] board
        case applyAction gs "p1" (ReserveCard (FromDisplay "c1")) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            gemCount (playerTokens p) GoldToken `shouldBe` 0
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "reserve from deck: top card removed" $ do
        let card1 = mkCard "c1" Tier1 [] Diamond 0
            card2 = mkCard "c2" Tier1 [] Ruby 0
            p1 = emptyPlayer "p1"
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [card1, card2] []
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
        case applyAction gs "p1" (ReserveCard (FromTopOfDeck Tier1)) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            length (playerReserved p) `shouldBe` 1
            -- Deck should have lost one card
            length (tierDeck (boardTier1 (gsBoard gs'))) `shouldBe` 1
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    -- ========== applyGemReturn tests ==========
    describe "applyGemReturn" $ do
      it "wrong phase: StepError" $ do
        let tokens = mkGems [(GemToken Ruby, 5)]
            p1 = playerWithTokens "p1" tokens
            gs = mkGameState [p1] (mkTestBoard twoPlayerBank [])
            -- AwaitingAction, not MustReturnGems
        case applyGemReturn gs "p1" (singleGem (GemToken Ruby) 1) of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

      it "wrong player: StepError NotYourTurn" $ do
        let tokens = mkGems [(GemToken Ruby, 6), (GemToken Diamond, 6)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            gs = (mkGameState [p1, p2] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 2 }
        case applyGemReturn gs "p2" (singleGem (GemToken Ruby) 2) of
          StepError NotYourTurn -> pure ()
          other -> expectationFailure $ "Expected StepError NotYourTurn, got: " ++ show other

      it "wrong count: StepError" $ do
        let tokens = mkGems [(GemToken Ruby, 6), (GemToken Diamond, 6)]
            p1 = playerWithTokens "p1" tokens
            gs = (mkGameState [p1] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 2 }
        case applyGemReturn gs "p1" (singleGem (GemToken Ruby) 1) of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

      it "insufficient gems: StepError" $ do
        let tokens = mkGems [(GemToken Ruby, 6), (GemToken Diamond, 5)]
            p1 = playerWithTokens "p1" tokens
            gs = (mkGameState [p1] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 1 }
        case applyGemReturn gs "p1" (singleGem (GemToken Emerald) 1) of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

      it "valid return: tokens to bank, turn finishes" $ do
        let tokens = mkGems [(GemToken Ruby, 6), (GemToken Diamond, 6)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            gs = (mkGameState [p1, p2] (mkTestBoard twoPlayerBank []))
                   { gsTurnPhase = MustReturnGems 2 }
        case applyGemReturn gs "p1" (mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1)]) of
          Advanced gs' -> do
            -- Player should have returned 2 gems
            let p = gsPlayers gs' !! 0
            gemCount (playerTokens p) (GemToken Ruby) `shouldBe` 5
            gemCount (playerTokens p) (GemToken Diamond) `shouldBe` 5
            -- Turn should advance to player 2
            gsCurrentPlayer gs' `shouldBe` 1
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    -- ========== applyNobleChoice tests ==========
    describe "applyNobleChoice" $ do
      it "valid choice: noble to player, removed from board" $ do
        let noble1 = mkNoble "n1" [(Diamond, 3)]
            noble2 = mkNoble "n2" [(Ruby, 3)]
            cards = [ mkCard ("d" <> T.pack (show i)) Tier1 [] Diamond 0 | i <- [1..3 :: Int] ]
            p1 = playerWithCards "p1" cards
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = [noble1, noble2]
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
        case applyNobleChoice gs "p1" "n1" of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            case playerNobles p of
              (n:_) -> nobleId n `shouldBe` "n1"
              []    -> expectationFailure "Expected at least one noble"
            -- Noble removed from board
            length (boardNobles (gsBoard gs')) `shouldBe` 1
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "noble not found: StepError" $ do
        let p1 = emptyPlayer "p1"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1] board
        case applyNobleChoice gs "p1" "n99" of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

      it "wrong player: StepError NotYourTurn" $ do
        let noble1 = mkNoble "n1" [(Diamond, 3)]
            p1 = emptyPlayer "p1"
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = [noble1]
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
        case applyNobleChoice gs "p2" "n1" of
          StepError NotYourTurn -> pure ()
          other -> expectationFailure $ "Expected StepError NotYourTurn, got: " ++ show other

    -- ========== Phase transition tests ==========
    describe "phase transitions" $ do
      it "InProgress -> FinalRound on 15+ prestige" $ do
        let existingCards = [ mkCard ("e" <> T.pack (show i)) Tier1 [] Diamond
                              (if i == 1 then 12 else 0)
                            | i <- [1..4 :: Int] ]
            bigCard = mkCard "big" Tier2 [(Ruby, 1)] Emerald 3
            tokens = mkGems [(GemToken Ruby, 2)]
            p1 = (playerWithTokens "p1" tokens) { playerPurchased = existingCards }
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] [bigCard]
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1)]
        case applyAction gs "p1" (BuyCard (FromDisplay "big") payment) of
          Advanced gs' -> gsPhase gs' `shouldBe` FinalRound
          GameOver _ _ -> pure ()  -- Also valid if round completes
          other -> expectationFailure $ "Expected FinalRound/GameOver, got: " ++ show other

      it "FinalRound -> GameOver when round completes (back to player 0)" $ do
        -- 2 players. Player 0 triggered final round. Player 1 takes gems.
        -- Then back to player 0 -> game over.
        let existingCards = [ mkCard ("e" <> T.pack (show i)) Tier1 [] Diamond
                              (if i == 1 then 15 else 0)
                            | i <- [1..4 :: Int] ]
            p1 = playerWithCards "p1" existingCards
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = (mkGameState [p1, p2] board)
                   { gsPhase = FinalRound
                   , gsCurrentPlayer = 1  -- Player 1's turn in final round
                   }
            -- Player 1 takes gems
            action = TakeGems (TakeDifferent [Ruby, Diamond, Emerald])
        case applyAction gs "p2" action of
          GameOver _gs' result -> do
            winnerId result `shouldBe` "p1"
            finalPrestige result `shouldBe` 15
          other -> expectationFailure $ "Expected GameOver, got: " ++ show other

      it "FinalRound continues mid-round" $ do
        -- 3 players. FinalRound, player 0's turn. Should continue to player 1.
        let existingCards = [ mkCard ("e" <> T.pack (show i)) Tier1 [] Diamond
                              (if i == 1 then 15 else 0)
                            | i <- [1..4 :: Int] ]
            p1 = playerWithCards "p1" existingCards
            p2 = emptyPlayer "p2"
            p3 = emptyPlayer "p3"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = mkGems $ [(GemToken c, 5) | c <- allGemColors] ++ [(GoldToken, 5)]
              }
            gs = (mkGameState [p1, p2, p3] board)
                   { gsPhase = FinalRound
                   , gsCurrentPlayer = 1  -- Player 1's turn mid-round
                   }
            action = TakeGems (TakeDifferent [Ruby, Diamond, Emerald])
        case applyAction gs "p2" action of
          Advanced gs' -> do
            gsCurrentPlayer gs' `shouldBe` 2  -- Continues to player 2
            gsPhase gs' `shouldBe` FinalRound
          other -> expectationFailure $ "Expected Advanced in FinalRound, got: " ++ show other

    -- ========== Noble auto-visit tests ==========
    describe "noble auto-visit" $ do
      it "one eligible noble after buy: auto-assigned" $ do
        let noble1 = mkNoble "n1" [(Diamond, 3)]
            -- Player already has 2 Diamond bonuses, buying a Diamond card will give 3
            existingCards = [ mkCard "d1" Tier1 [] Diamond 0
                            , mkCard "d2" Tier1 [] Diamond 0
                            ]
            -- New Diamond card to buy (cost: 1 Ruby)
            newCard = mkCard "newD" Tier1 [(Ruby, 1)] Diamond 0
            tokens = mkGems [(GemToken Ruby, 2)]
            p1 = (playerWithTokens "p1" tokens) { playerPurchased = existingCards }
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] [newCard]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = [noble1]
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1)]
        case applyAction gs "p1" (BuyCard (FromDisplay "newD") payment) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            case playerNobles p of
              (n:_) -> nobleId n `shouldBe` "n1"
              []    -> expectationFailure "Expected at least one noble"
            -- Noble removed from board
            boardNobles (gsBoard gs') `shouldBe` []
          other -> expectationFailure $ "Expected Advanced with noble, got: " ++ show other

      it "multiple eligible nobles: NeedNobleChoice" $ do
        let noble1 = mkNoble "n1" [(Diamond, 3)]
            noble2 = mkNoble "n2" [(Diamond, 3)]  -- Same requirement
            existingCards = [ mkCard "d1" Tier1 [] Diamond 0
                            , mkCard "d2" Tier1 [] Diamond 0
                            ]
            newCard = mkCard "newD" Tier1 [(Ruby, 1)] Diamond 0
            tokens = mkGems [(GemToken Ruby, 2)]
            p1 = (playerWithTokens "p1" tokens) { playerPurchased = existingCards }
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] [newCard]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = [noble1, noble2]
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1)]
        case applyAction gs "p1" (BuyCard (FromDisplay "newD") payment) of
          NeedNobleChoice _ nobles -> length nobles `shouldBe` 2
          other -> expectationFailure $ "Expected NeedNobleChoice, got: " ++ show other

      it "none eligible: normal advance" $ do
        let noble1 = mkNoble "n1" [(Ruby, 5)]  -- Requires 5 Ruby bonuses
            card1 = mkCard "c1" Tier1 [(Ruby, 1)] Diamond 0
            tokens = mkGems [(GemToken Ruby, 2)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] [card1]
              , boardTier2 = mkTierRow [] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = [noble1]
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          Advanced gs' -> do
            let p = gsPlayers gs' !! 0
            playerNobles p `shouldBe` []
            -- Noble still on board
            length (boardNobles (gsBoard gs')) `shouldBe` 1
          other -> expectationFailure $ "Expected Advanced without noble, got: " ++ show other

    -- ========== BuyCard with gold payment (full pipeline) ==========
    describe "applyAction - BuyCard with gold payment" $ do
      it "gold covers shortfall on one color" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 3), (Diamond, 1)] Emerald 1
            -- Player has 1 Ruby + 2 Gold — gold covers 2 Ruby shortfall
            tokens = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1), (GoldToken, 2)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1), (GoldToken, 2)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          Advanced gs' -> case gsPlayers gs' of
            (p:_) -> do
              -- Player should have 0 tokens left
              totalGems (playerTokens p) `shouldBe` 0
              -- Card should be purchased
              case playerPurchased p of
                (c:_) -> cardId c `shouldBe` "c1"
                []    -> expectationFailure "Expected purchased card"
              -- Bank gets payment back
              gemCount (boardBank (gsBoard gs')) GoldToken `shouldBe` 7  -- 5 + 2
            [] -> expectationFailure "Expected players"
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "gold covers shortfalls across multiple colors" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 2), (Diamond, 2)] Emerald 0
            -- Player has 1 Ruby + 1 Diamond + 2 Gold — gold covers 1 Ruby + 1 Diamond
            tokens = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1), (GoldToken, 2)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1), (GemToken Diamond, 1), (GoldToken, 2)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          Advanced gs' -> case gsPlayers gs' of
            (p:_) -> do
              totalGems (playerTokens p) `shouldBe` 0
              length (playerPurchased p) `shouldBe` 1
            [] -> expectationFailure "Expected players"
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "gold overpayment rejected" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 3)] Emerald 0
            -- Player has 1 Ruby + 3 Gold — shortfall is 2 Ruby, but pays 3 gold
            tokens = mkGems [(GemToken Ruby, 1), (GoldToken, 3)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1), (GoldToken, 3)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

      it "gold underpayment rejected" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 3)] Emerald 0
            -- Player has 1 Ruby + 1 Gold — shortfall is 2 Ruby, but only pays 1 gold
            tokens = mkGems [(GemToken Ruby, 1), (GoldToken, 1)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1), (GoldToken, 1)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

      it "gold with zero shortfall rejected" $ do
        let card1 = mkCard "c1" Tier1 [(Ruby, 2)] Emerald 0
            -- Player has 2 Ruby + 1 Gold — no shortfall, but tries to pay gold anyway
            tokens = mkGems [(GemToken Ruby, 2), (GoldToken, 1)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 2), (GoldToken, 1)]
        case applyAction gs "p1" (BuyCard (FromDisplay "c1") payment) of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

    -- ========== ReserveCard + gold → >10 tokens → NeedGemReturn ==========
    describe "applyAction - ReserveCard triggering NeedGemReturn" $ do
      it "reserve + gold pushes to >10 tokens → NeedGemReturn" $ do
        let card1 = mkCard "c1" Tier1 [] Diamond 0
            -- Player has exactly 10 tokens; reserving gives gold → 11 → must return 1
            tokens = mkGems [(GemToken Ruby, 5), (GemToken Diamond, 5)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank [card1]
            gs = mkGameState [p1, p2] board
        case applyAction gs "p1" (ReserveCard (FromDisplay "c1")) of
          NeedGemReturn gs' excess -> do
            excess `shouldBe` 1
            -- Player should have card reserved
            case gsPlayers gs' of
              (p:_) -> do
                length (playerReserved p) `shouldBe` 1
                -- Player should have 11 tokens (including gold)
                totalGems (playerTokens p) `shouldBe` 11
              [] -> expectationFailure "Expected players"
          other -> expectationFailure $ "Expected NeedGemReturn, got: " ++ show other

      it "reserve without gold at 10 tokens → Advanced (no excess)" $ do
        let card1 = mkCard "c1" Tier1 [] Diamond 0
            tokens = mkGems [(GemToken Ruby, 5), (GemToken Diamond, 5)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            -- No gold in bank
            bank = mkGems [(GemToken Ruby, 4), (GemToken Diamond, 4), (GemToken Emerald, 4),
                           (GemToken Sapphire, 4), (GemToken Onyx, 4)]
            board = mkTestBoard bank [card1]
            gs = mkGameState [p1, p2] board
        case applyAction gs "p1" (ReserveCard (FromDisplay "c1")) of
          Advanced gs' -> case gsPlayers gs' of
            (p:_) ->
              -- No gold received, still at 10
              totalGems (playerTokens p) `shouldBe` 10
            [] -> expectationFailure "Expected players"
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    -- ========== BuyCard from FromTopOfDeck ==========
    describe "applyAction - BuyCard from FromTopOfDeck" $ do
      it "buys top card from tier 1 deck" $ do
        let deckCard = mkCard "deck1" Tier1 [(Ruby, 1)] Diamond 0
            tokens = mkGems [(GemToken Ruby, 2)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoardWithDeck twoPlayerBank [] [deckCard]
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1)]
        case applyAction gs "p1" (BuyCard (FromTopOfDeck Tier1) payment) of
          Advanced gs' -> case gsPlayers gs' of
            (p:_) -> do
              case playerPurchased p of
                (c:_) -> cardId c `shouldBe` "deck1"
                []    -> expectationFailure "Expected purchased card"
              -- Deck should be empty now
              length (tierDeck (boardTier1 (gsBoard gs'))) `shouldBe` 0
            [] -> expectationFailure "Expected players"
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

      it "buying from empty deck returns StepError" $ do
        let tokens = mkGems [(GemToken Ruby, 2)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = mkTestBoard twoPlayerBank []
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Ruby, 1)]
        case applyAction gs "p1" (BuyCard (FromTopOfDeck Tier1) payment) of
          StepError _ -> pure ()
          other -> expectationFailure $ "Expected StepError, got: " ++ show other

      it "buys from tier 2 deck" $ do
        let deckCard = mkCard "t2deck" Tier2 [(Diamond, 1)] Ruby 2
            tokens = mkGems [(GemToken Diamond, 2)]
            p1 = playerWithTokens "p1" tokens
            p2 = emptyPlayer "p2"
            board = Board
              { boardTier1 = mkTierRow [] []
              , boardTier2 = mkTierRow [deckCard] []
              , boardTier3 = mkTierRow [] []
              , boardNobles = []
              , boardBank = twoPlayerBank
              }
            gs = mkGameState [p1, p2] board
            payment = mkGems [(GemToken Diamond, 1)]
        case applyAction gs "p1" (BuyCard (FromTopOfDeck Tier2) payment) of
          Advanced gs' -> case gsPlayers gs' of
            (p:_) -> do
              case playerPurchased p of
                (c:_) -> cardPrestige c `shouldBe` 2
                []    -> expectationFailure "Expected purchased card"
            [] -> expectationFailure "Expected players"
          other -> expectationFailure $ "Expected Advanced, got: " ++ show other

    -- ========== Full game simulations ==========
    describe "full game simulation" $ do
      it "completes a 2-player random game within 500 turns" $ do
        let gs = initGameState (mkStdGen 42) "test" 2 ["Alice", "Bob"]
            gen = mkStdGen 123
            result = simulateGame gs gen 500
        result `shouldSatisfy` \case
          GameOver _ _ -> True
          _            -> False

      it "completes a 3-player random game within 500 turns" $ do
        let gs = initGameState (mkStdGen 77) "test" 3 ["A", "B", "C"]
            gen = mkStdGen 456
            result = simulateGame gs gen 500
        result `shouldSatisfy` \case
          GameOver _ _ -> True
          _            -> False

    -- ========== QuickCheck properties ==========
    describe "QuickCheck properties" $ do
      it "token conservation across random game simulation" $ property $ \seed ->
        let gs = initGameState (mkStdGen seed) "test" 2 ["A", "B"]
            gen = mkStdGen (seed + 1)
        in case simulateGame gs gen 200 of
             StepError (OtherError msg) -> msg /= "Token conservation violated!"
             _ -> True

      it "all legalActions pass validateAction for random initial states" $ property $ \seed ->
        let gs = initGameState (mkStdGen seed) "test" 2 ["A", "B"]
            actions = legalActions gs
            pid = playerId (fromJust (currentPlayer gs))
        in all (\a -> isRight (validateAction gs pid a)) actions

-- ========== Test helpers ==========


-- | Compute total tokens in the game (bank + all player hands)
totalTokens :: GameState -> GemCollection
totalTokens gs =
  foldl addGems (boardBank (gsBoard gs)) (map playerTokens (gsPlayers gs))

-- | Handle a NeedNobleChoice result by randomly picking a noble
handleNobleChoice :: GameState -> PlayerId -> [Noble] -> StdGen -> (StepResult, StdGen)
handleNobleChoice gs pid nobles g =
  let (nIdx, g') = uniformR (0, length nobles - 1) g
      noble = nobles !! nIdx
  in case applyNobleChoice gs pid (nobleId noble) of
       Advanced gs' -> (Advanced gs', g')
       GameOver gs' result -> (GameOver gs' result, g')
       other -> (other, g')

-- | Simulate a game by picking random legal actions until completion or turn limit.
-- Also checks token conservation after every step.
simulateGame :: GameState -> StdGen -> Int -> StepResult
simulateGame gs gen maxTurns =
  let initialTotal = totalTokens gs
  in go initialTotal gs gen maxTurns
  where
    go _ gs' _ 0 = Advanced gs'  -- Ran out of turns
    go initTok gs' g n = case gsPhase gs' of
      Finished result -> GameOver gs' result
      _ -> case gsTurnPhase gs' of
        MustReturnGems _ ->
          let returns = legalGemReturns gs'
          in if null returns
             then StepError (OtherError "No legal gem returns")
             else
               let (idx, g') = uniformR (0, length returns - 1) g
                   ret = returns !! idx
                   pid = playerId (fromJust (currentPlayer gs'))
               in case applyGemReturn gs' pid ret of
                    Advanced gs'' -> checkAndContinue initTok gs'' g' n
                    NeedNobleChoice gs'' nobles ->
                      let (result, g'') = handleNobleChoice gs'' pid nobles g'
                      in case result of
                           Advanced gs''' -> checkAndContinue initTok gs''' g'' n
                           other -> other
                    GameOver gs'' result -> GameOver gs'' result
                    other -> other
        AwaitingAction ->
          let actions = legalActions gs'
          in if null actions
             then StepError (OtherError "No legal actions")
             else
               let (idx, g') = uniformR (0, length actions - 1) g
                   action = actions !! idx
                   pid = playerId (fromJust (currentPlayer gs'))
               in case applyAction gs' pid action of
                    Advanced gs'' -> checkAndContinue initTok gs'' g' n
                    NeedGemReturn gs'' _ -> checkAndContinue initTok gs'' g' n
                    NeedNobleChoice gs'' nobles ->
                      let (result, g'') = handleNobleChoice gs'' pid nobles g'
                      in case result of
                           Advanced gs''' -> checkAndContinue initTok gs''' g'' n
                           other -> other
                    GameOver gs'' result -> GameOver gs'' result
                    other -> other

    checkAndContinue initTok gs' g n
      | totalTokens gs' /= initTok =
          StepError (OtherError "Token conservation violated!")
      | otherwise = go initTok gs' g (n - 1)
