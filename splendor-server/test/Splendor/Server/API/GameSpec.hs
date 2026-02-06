module Splendor.Server.API.GameSpec (spec) where

import Control.Concurrent.STM (modifyTVar', atomically)
import Servant (Handler, ServerError(..), (:<|>)(..))
import Servant qualified
import Test.Hspec

import Splendor.Core.Types
import Splendor.Server.API.Game (gameServer)
import Splendor.Server.GameManager (processAction)
import Splendor.Server.TestHelpers
import Splendor.Server.Types

spec :: Spec
spec = do
  describe "getGameHandler" $ do
    it "valid session returns PublicGameView with correct gameId" $ do
      (ss, gid, s1, _) <- setupGame
      view <- run $ getGameH ss gid s1
      pgvGameId view `shouldBe` gid

    it "view contains correct player count and names" $ do
      (ss, gid, s1, _) <- setupGame
      view <- run $ getGameH ss gid s1
      length (pgvPlayers view) `shouldBe` 2
      map ppPlayerName (pgvPlayers view) `shouldBe` ["Alice", "Bob"]

    it "view shows own reserved, hides opponent's" $ do
      (ss, gid, s1, s2) <- setupGame
      gameTVar <- lookupGameTVarOrFail ss gid
      -- Give player 0 a reserved card
      atomically $ modifyTVar' gameTVar $ \mg ->
        let gs = mgGameState mg
            ps = gsPlayers gs
            p0 = (ps !! 0) { playerReserved = [Card "r1" Tier1 emptyGems Ruby 0] }
            gs' = gs { gsPlayers = p0 : drop 1 ps }
        in mg { mgGameState = gs' }
      -- Player 0's view: sees own reserved
      view1 <- run $ getGameH ss gid s1
      let pp0_from_s1 = pgvPlayers view1 !! 0
      ppReserved pp0_from_s1 `shouldBe` Just [Card "r1" Tier1 emptyGems Ruby 0]
      -- Player 1's view: doesn't see player 0's reserved
      view2 <- run $ getGameH ss gid s2
      let pp0_from_s2 = pgvPlayers view2 !! 0
      ppReserved pp0_from_s2 `shouldBe` Nothing
      ppReservedCount pp0_from_s2 `shouldBe` 1

    it "view reflects game state after an action" $ do
      (ss, gid, s1, s2) <- setupGame
      session <- currentSession ss gid s1 s2
      _ <- processAction ss gid session (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
      view <- run $ getGameH ss gid s1
      -- Turn number or current player should have changed
      pgvCurrentPlayer view `shouldBe` 1  -- After first action, it's player 1's turn

    it "nonexistent game returns 404" $ do
      (ss, _, _, _) <- setupGame
      result <- Servant.runHandler $ getGameH ss "nonexistent-game" "session-1"
      case result of
        Left err -> errHTTPCode err `shouldBe` 404
        Right _  -> expectationFailure "Expected 404 error"

    it "invalid session returns 403" $ do
      (ss, gid, _, _) <- setupGame
      result <- Servant.runHandler $ getGameH ss gid "bad-session"
      case result of
        Left err -> errHTTPCode err `shouldBe` 403
        Right _  -> expectationFailure "Expected 403 error"

    it "board shows public deck counts, not card contents" $ do
      (ss, gid, s1, _) <- setupGame
      view <- run $ getGameH ss gid s1
      let board = pgvBoard view
      -- Public tier rows should have deck counts (not the actual cards)
      -- The deck count should be >= 0
      publicDeckCount (publicTier1 board) `shouldSatisfy` (>= 0)
      publicDeckCount (publicTier2 board) `shouldSatisfy` (>= 0)
      publicDeckCount (publicTier3 board) `shouldSatisfy` (>= 0)
      -- The display cards should be visible (4 per tier in standard setup)
      length (publicDisplay (publicTier1 board)) `shouldSatisfy` (> 0)

-- ============================================================
-- Handler extraction
-- ============================================================

getGameH :: ServerState -> GameId -> SessionId -> Handler PublicGameView
getGameH ss = let (h :<|> _) = gameServer ss in h

-- | Run a Handler, failing the test on ServerError.
run :: Handler a -> IO a
run h = do
  result <- Servant.runHandler h
  case result of
    Right a  -> pure a
    Left err -> error $ "Handler failed: " ++ show err
