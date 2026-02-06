module Splendor.Server.API.LobbySpec (spec) where

import Control.Concurrent.STM (readTVarIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Servant (Handler, ServerError(..), (:<|>)(..))
import Servant qualified
import Test.Hspec
import Splendor.Server.Types
import Splendor.Server.API.Lobby (lobbyServer)

spec :: Spec
spec = do
  describe "createLobbyHandler" $ do
    it "returns lobbyId and sessionId" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      clrLobbyId resp `shouldSatisfy` (not . T.null)
      clrSessionId resp `shouldSatisfy` (not . T.null)

    it "lobby appears in ssLobbies with correct name and 1 slot" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      lobbies <- readTVarIO (ssLobbies ss)
      case Map.lookup (clrLobbyId resp) lobbies of
        Nothing -> expectationFailure "Lobby not found in ssLobbies"
        Just lobby -> do
          lobbyName lobby `shouldBe` "My Lobby"
          length (lobbySlots lobby) `shouldBe` 1

  describe "listLobbiesHandler" $ do
    it "empty state returns empty list" $ do
      ss <- newServerState
      result <- run $ listH ss
      result `shouldBe` []

    it "after create, list contains the lobby" $ do
      ss <- newServerState
      _ <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      result <- run $ listH ss
      length result `shouldBe` 1

  describe "getLobbyHandler" $ do
    it "valid id returns lobby" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      lobby <- run $ getH ss (clrLobbyId resp)
      lobbyName lobby `shouldBe` "My Lobby"

    it "invalid id throws 404" $ do
      ss <- newServerState
      result <- Servant.runHandler $ getH ss "nonexistent"
      case result of
        Left err -> errHTTPCode err `shouldBe` 404
        Right _  -> expectationFailure "Expected 404 error"

  describe "joinLobbyHandler" $ do
    it "valid join adds slot, returns sessionId" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      joinResp <- run $ joinH ss (clrLobbyId resp) (JoinLobbyRequest "Bob")
      jlrSessionId joinResp `shouldSatisfy` (not . T.null)
      lobbies <- readTVarIO (ssLobbies ss)
      case Map.lookup (clrLobbyId resp) lobbies of
        Nothing -> expectationFailure "Lobby not found"
        Just lobby -> length (lobbySlots lobby) `shouldBe` 2

    it "full lobby throws 400" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      let lid = clrLobbyId resp
      _ <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      _ <- run $ joinH ss lid (JoinLobbyRequest "Charlie")
      _ <- run $ joinH ss lid (JoinLobbyRequest "Dave")
      result <- Servant.runHandler $ joinH ss lid (JoinLobbyRequest "Eve")
      case result of
        Left err -> errHTTPCode err `shouldBe` 400
        Right _  -> expectationFailure "Expected 400 error"

  describe "startGameHandler" $ do
    it "2+ players creates game, lobby status becomes Started" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      let lid = clrLobbyId resp
      _ <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      startResp <- run $ startH ss lid
      sgrGameId startResp `shouldSatisfy` (not . T.null)
      lobbies <- readTVarIO (ssLobbies ss)
      case Map.lookup lid lobbies of
        Nothing -> expectationFailure "Lobby not found"
        Just lobby -> case lobbyStatus lobby of
          Started _ -> pure ()
          other     -> expectationFailure $ "Expected Started, got: " ++ show other

    it "1 player throws 400" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      result <- Servant.runHandler $ startH ss (clrLobbyId resp)
      case result of
        Left err -> errHTTPCode err `shouldBe` 400
        Right _  -> expectationFailure "Expected 400 error"

    it "second start attempt fails (TOCTOU prevention)" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      let lid = clrLobbyId resp
      _ <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      _ <- run $ startH ss lid
      result <- Servant.runHandler $ startH ss lid
      case result of
        Left err -> errHTTPCode err `shouldBe` 400
        Right _  -> expectationFailure "Expected 400 on second start"

    it "lobby status transitions through Starting to Started" $ do
      ss <- newServerState
      resp <- run $ createH ss (CreateLobbyRequest "Alice" "My Lobby")
      let lid = clrLobbyId resp
      _ <- run $ joinH ss lid (JoinLobbyRequest "Bob")
      _ <- run $ startH ss lid
      lobbies <- readTVarIO (ssLobbies ss)
      case Map.lookup lid lobbies of
        Nothing -> expectationFailure "Lobby not found"
        Just lobby -> case lobbyStatus lobby of
          Started _ -> pure ()
          other     -> expectationFailure $ "Expected Started, got: " ++ show other

-- ============================================================
-- Handler extraction
-- ============================================================

createH :: ServerState -> CreateLobbyRequest -> Handler CreateLobbyResponse
createH ss = let (h :<|> _) = lobbyServer ss in h

listH :: ServerState -> Handler [Lobby]
listH ss = let (_ :<|> h :<|> _) = lobbyServer ss in h

getH :: ServerState -> LobbyId -> Handler Lobby
getH ss = let (_ :<|> _ :<|> h :<|> _) = lobbyServer ss in h

joinH :: ServerState -> LobbyId -> JoinLobbyRequest -> Handler JoinLobbyResponse
joinH ss = let (_ :<|> _ :<|> _ :<|> h :<|> _) = lobbyServer ss in h

startH :: ServerState -> LobbyId -> Handler StartGameResponse
startH ss = let (_ :<|> _ :<|> _ :<|> _ :<|> h) = lobbyServer ss in h

-- | Run a Handler, failing the test on ServerError
run :: Handler a -> IO a
run h = do
  result <- Servant.runHandler h
  case result of
    Right a  -> pure a
    Left err -> error $ "Handler failed: " ++ show err
