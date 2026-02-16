module Splendor.Server.PersistenceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.SQLite.Simple
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Hspec

import Splendor.Core.Types
import Splendor.Server.GameManager (createGame)
import Splendor.Server.Persistence
import Splendor.Server.Persistence.Schema (initSchema)
import Splendor.Server.TestHelpers
import Splendor.Server.Types

spec :: Spec
spec = do
  describe "initPersistence" $ do
    it "Nothing returns NoPersistence" $ do
      ph <- initPersistence Nothing
      case ph of
        NoPersistence -> pure ()
        PersistenceHandle _ -> expectationFailure "Expected NoPersistence"

    it "Just filepath returns PersistenceHandle" $ do
      tmpDir <- getCanonicalTemporaryDirectory
      dir <- createTempDirectory tmpDir "splendor-test-init"
      let dbPath = dir </> "test.db"
      ph <- initPersistence (Just dbPath)
      case ph of
        PersistenceHandle _ -> pure ()
        NoPersistence -> expectationFailure "Expected PersistenceHandle"
      removeDirectoryRecursive dir

  describe "initSchema" $ do
    it "creates tables on fresh database" $ withTempDb $ \ph -> do
      case ph of
        PersistenceHandle conn -> do
          rows <- query_ conn
            "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
            :: IO [[String]]
          let tableNames = map (\case [n] -> n; _ -> "") rows
          tableNames `shouldContain` ["games"]
          tableNames `shouldNotContain` ["sessions"]
        NoPersistence -> expectationFailure "Expected PersistenceHandle"

    it "is idempotent — calling twice doesn't error" $ withTempDb $ \ph -> do
      initSchema ph
      -- If we get here without exception, it passed
      pure ()

    it "NoPersistence is a no-op" $ do
      initSchema NoPersistence
      -- Should not throw
      pure ()

  describe "saveGame / loadAllActiveGames round-trip" $ do
    it "save then load recovers identical GameState" $ withTempDb $ \ph -> do
      (ss, gid, _, _) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      let gs = mgGameState mg
      loaded <- loadAllActiveGames ph
      case loaded of
        [(_, gs', _, _)] -> gs' `shouldBe` gs
        _ -> expectationFailure $ "Expected exactly 1 game, got " ++ show (length loaded)

    it "save then load recovers identical PlayerSession map" $ withTempDb $ \ph -> do
      (ss, gid, _, _) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      let sessions = mgSessions mg
      loaded <- loadAllActiveGames ph
      case loaded of
        [(_, _, sess', _)] -> sess' `shouldBe` sessions
        _ -> expectationFailure $ "Expected exactly 1 game, got " ++ show (length loaded)

    it "pendingNobles = Nothing round-trips as Nothing" $ withTempDb $ \ph -> do
      (ss, gid, _, _) <- setupGameWithPersistence ph
      _mg <- lookupGameOrFail ss gid
      -- createGame sets mgPendingNobles = Nothing, so it should be persisted as Nothing
      loaded <- loadAllActiveGames ph
      case loaded of
        [(_, _, _, nobles)] -> nobles `shouldBe` Nothing
        _ -> expectationFailure "Expected exactly 1 game"

    it "pendingNobles = Just [nobles] round-trips correctly" $ withTempDb $ \ph -> do
      (ss, gid, _, _) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      let noble1 = Noble "n1" (Map.fromList [(Diamond, 3)]) 3
          noble2 = Noble "n2" (Map.fromList [(Ruby, 2), (Emerald, 2)]) 3
          pendingNobles = Just [noble1, noble2]
      -- Re-save with pendingNobles set
      saveGame ph gid (mgGameState mg) GameActive (mgSessions mg) pendingNobles
      loaded <- loadAllActiveGames ph
      case loaded of
        [(_, _, _, nobles')] -> nobles' `shouldBe` pendingNobles
        _ -> expectationFailure "Expected exactly 1 game"

    it "GameFinished games are excluded from loadAllActiveGames" $ withTempDb $ \ph -> do
      (ss, gid, _, _) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      -- Re-save as finished
      saveGame ph gid (mgGameState mg) GameFinished (mgSessions mg) Nothing
      loaded <- loadAllActiveGames ph
      loaded `shouldBe` []

    it "UPSERT: re-saving same game ID overwrites (only 1 entry returned)" $ withTempDb $ \ph -> do
      (ss, gid, _, _) <- setupGameWithPersistence ph
      mg <- lookupGameOrFail ss gid
      -- Save again
      saveGame ph gid (mgGameState mg) GameActive (mgSessions mg) Nothing
      loaded <- loadAllActiveGames ph
      length loaded `shouldBe` 1

    it "multiple games coexist in database" $ withTempDb $ \ph -> do
      ss <- newServerState ph
      let slots1 = [ LobbySlot "s1" "Alice" False, LobbySlot "s2" "Bob" False ]
          slots2 = [ LobbySlot "s3" "Charlie" False, LobbySlot "s4" "Diana" False ]
      _gid1 <- createGame ss slots1
      _gid2 <- createGame ss slots2
      loaded <- loadAllActiveGames ph
      length loaded `shouldBe` 2

  describe "NoPersistence" $ do
    it "saveGame NoPersistence is a no-op" $ do
      let gs = error "should not evaluate GameState for NoPersistence"
          sessions = error "should not evaluate sessions for NoPersistence"
      saveGame NoPersistence "gid" gs GameActive sessions Nothing
      -- If we get here without forcing the errors, it passed
      pure ()

    it "loadAllActiveGames NoPersistence returns []" $ do
      loaded <- loadAllActiveGames NoPersistence
      loaded `shouldBe` []

  describe "corrupt data" $ do
    it "loadAllActiveGames silently skips rows with invalid JSON" $ withTempDb $ \ph -> do
      case ph of
        PersistenceHandle conn -> do
          -- Insert a valid game first
          (_ss, gid, _, _) <- setupGameWithPersistence ph
          -- Insert a row with corrupt JSON directly
          execute conn
            "INSERT INTO games (game_id, status, game_state, sessions, pending_nobles, updated_at) \
            \VALUES (?, 'active', ?, ?, NULL, datetime('now'))"
            ("corrupt-game" :: T.Text, "not-valid-json" :: T.Text, "also-not-json" :: T.Text)
          loaded <- loadAllActiveGames ph
          -- Should only return the valid game, not crash
          length loaded `shouldBe` 1
          case loaded of
            [(gid', _, _, _)] -> gid' `shouldBe` gid
            _ -> expectationFailure "Expected exactly the valid game"
        NoPersistence -> expectationFailure "Expected PersistenceHandle"
