module Splendor.Core.EngineSpec (spec) where

import Data.Maybe (fromJust)
import System.Random (mkStdGen, uniformR, StdGen)
import Test.Hspec
import Splendor.Core.Types
import Splendor.Core.Engine
import Splendor.Core.Setup (initGameState)
import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)

spec :: Spec
spec = do
  describe "Engine" $ do
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
            -- Pick the first gem-take action
            gemAction = head [ a | a@(TakeGems _) <- actions ]
            pid = playerId (fromJust (currentPlayer gs))
            result = applyAction gs pid gemAction
        case result of
          Advanced gs' -> gsCurrentPlayer gs' `shouldBe` 1
          NeedGemReturn _ _ -> pure ()  -- Also valid
          other -> expectationFailure $ "Expected Advanced or NeedGemReturn, got: " ++ show other

    describe "full game simulation" $ do
      it "completes a 2-player random game within 500 turns" $ do
        let gs = initGameState (mkStdGen 42) "test" 2 ["Alice", "Bob"]
            gen = mkStdGen 123
        result <- simulateGame gs gen 500
        result `shouldSatisfy` \case
          GameOver _ _ -> True
          _            -> False

      it "completes a 3-player random game within 500 turns" $ do
        let gs = initGameState (mkStdGen 77) "test" 3 ["A", "B", "C"]
            gen = mkStdGen 456
        result <- simulateGame gs gen 500
        result `shouldSatisfy` \case
          GameOver _ _ -> True
          _            -> False

-- | Simulate a game by picking random legal actions until completion or turn limit
simulateGame :: GameState -> StdGen -> Int -> IO StepResult
simulateGame gs gen maxTurns = go gs gen maxTurns
  where
    go gs' _ 0 = pure (Advanced gs')  -- Ran out of turns
    go gs' g n = case gsPhase gs' of
      Finished result -> pure (GameOver gs' result)
      _ -> case gsTurnPhase gs' of
        MustReturnGems _ ->
          let returns = legalGemReturns gs'
          in if null returns
             then pure (StepError (OtherError "No legal gem returns"))
             else do
               let (idx, g') = uniformR (0, length returns - 1) g
                   ret = returns !! idx
                   pid = playerId (fromJust (currentPlayer gs'))
               case applyGemReturn gs' pid ret of
                 Advanced gs'' -> go gs'' g' (n - 1)
                 NeedGemReturn gs'' _ -> go gs'' g' (n - 1)
                 NeedNobleChoice gs'' nobles ->
                   let (nIdx, g'') = uniformR (0, length nobles - 1) g'
                       noble = nobles !! nIdx
                   in case applyNobleChoice gs'' pid (nobleId noble) of
                        Advanced gs''' -> go gs''' g'' (n - 1)
                        GameOver gs''' result -> pure (GameOver gs''' result)
                        other -> pure other
                 GameOver gs'' result -> pure (GameOver gs'' result)
                 other -> pure other
        AwaitingAction ->
          let actions = legalActions gs'
          in if null actions
             then pure (StepError (OtherError "No legal actions"))
             else do
               let (idx, g') = uniformR (0, length actions - 1) g
                   action = actions !! idx
                   pid = playerId (fromJust (currentPlayer gs'))
               case applyAction gs' pid action of
                 Advanced gs'' -> go gs'' g' (n - 1)
                 NeedGemReturn gs'' _ -> go gs'' g' (n - 1)
                 NeedNobleChoice gs'' nobles ->
                   let (nIdx, g'') = uniformR (0, length nobles - 1) g'
                       noble = nobles !! nIdx
                   in case applyNobleChoice gs'' pid (nobleId noble) of
                        Advanced gs''' -> go gs''' g'' (n - 1)
                        GameOver gs''' result -> pure (GameOver gs''' result)
                        other -> pure other
                 GameOver gs'' result -> pure (GameOver gs'' result)
                 other -> pure other
