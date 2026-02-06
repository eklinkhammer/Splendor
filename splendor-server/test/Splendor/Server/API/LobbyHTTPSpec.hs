module Splendor.Server.API.LobbyHTTPSpec (spec) where

import Data.Aeson (decode)
import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (status200, status400, status404)
import Network.Wai.Test (simpleBody, simpleStatus)
import Test.Hspec

import Splendor.Server.TestHelpers
import Splendor.Server.Types

spec :: Spec
spec = describe "Lobby HTTP endpoints" $ do

  describe "POST /api/v1/lobbies" $ do
    it "creates a lobby with valid JSON" $ do
      (_, app) <- testAppWithState
      resp <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "Test Lobby")
      simpleStatus resp `shouldBe` status200
      let mBody = decode (simpleBody resp) :: Maybe CreateLobbyResponse
      mBody `shouldSatisfy` \case
        Just r  -> clrLobbyId r /= "" && clrSessionId r /= ""
        Nothing -> False

    it "returns 400 for empty body" $ do
      (_, app) <- testAppWithState
      resp <- postJSON app "/api/v1/lobbies" ("" :: String)
      simpleStatus resp `shouldBe` status400

    it "returns 400 for malformed JSON" $ do
      (_, app) <- testAppWithState
      resp <- postJSON app "/api/v1/lobbies" (42 :: Int)
      simpleStatus resp `shouldBe` status400

  describe "GET /api/v1/lobbies" $ do
    it "returns empty list when no lobbies exist" $ do
      (_, app) <- testAppWithState
      resp <- getJSON app "/api/v1/lobbies"
      simpleStatus resp `shouldBe` status200
      let mBody = decode (simpleBody resp) :: Maybe [Lobby]
      mBody `shouldBe` Just []

    it "returns 1 lobby after creation" $ do
      (_, app) <- testAppWithState
      _ <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "My Lobby")
      resp <- getJSON app "/api/v1/lobbies"
      simpleStatus resp `shouldBe` status200
      let mBody = decode (simpleBody resp) :: Maybe [Lobby]
      case mBody of
        Just lobbies -> length lobbies `shouldBe` 1
        Nothing      -> expectationFailure "Failed to decode lobby list"

  describe "GET /api/v1/lobbies/:id" $ do
    it "returns lobby for valid ID" $ do
      (_, app) <- testAppWithState
      createResp <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "My Lobby")
      case decode (simpleBody createResp) :: Maybe CreateLobbyResponse of
        Nothing -> expectationFailure "Failed to decode create response"
        Just cr -> do
          let lid = clrLobbyId cr
          resp <- getJSON app ("/api/v1/lobbies/" <> TE.encodeUtf8 lid)
          simpleStatus resp `shouldBe` status200
          let mLobby = decode (simpleBody resp) :: Maybe Lobby
          case mLobby of
            Just lobby -> lobbyName lobby `shouldBe` "My Lobby"
            Nothing    -> expectationFailure "Failed to decode lobby"

    it "returns 404 for invalid ID" $ do
      (_, app) <- testAppWithState
      resp <- getJSON app "/api/v1/lobbies/nonexistent-id"
      simpleStatus resp `shouldBe` status404

  describe "POST /api/v1/lobbies/:id/join" $ do
    it "joins a lobby successfully" $ do
      (_, app) <- testAppWithState
      createResp <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "My Lobby")
      case decode (simpleBody createResp) :: Maybe CreateLobbyResponse of
        Nothing -> expectationFailure "Failed to decode create response"
        Just cr -> do
          let lid = clrLobbyId cr
          resp <- postJSON app (joinPath lid) (JoinLobbyRequest "Bob")
          simpleStatus resp `shouldBe` status200
          let mJoin = decode (simpleBody resp) :: Maybe JoinLobbyResponse
          mJoin `shouldSatisfy` \case
            Just r  -> jlrSessionId r /= ""
            Nothing -> False

    it "returns 400 when lobby is full" $ do
      (_, app) <- testAppWithState
      createResp <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "My Lobby")
      case decode (simpleBody createResp) :: Maybe CreateLobbyResponse of
        Nothing -> expectationFailure "Failed to decode create response"
        Just cr -> do
          let lid = clrLobbyId cr
          _ <- postJSON app (joinPath lid) (JoinLobbyRequest "Bob")
          _ <- postJSON app (joinPath lid) (JoinLobbyRequest "Charlie")
          _ <- postJSON app (joinPath lid) (JoinLobbyRequest "Dana")
          -- 5th player should fail
          resp <- postJSON app (joinPath lid) (JoinLobbyRequest "Eve")
          simpleStatus resp `shouldBe` status400

  describe "POST /api/v1/lobbies/:id/start" $ do
    it "starts game with 2 players" $ do
      (_, app) <- testAppWithState
      createResp <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "My Lobby")
      case decode (simpleBody createResp) :: Maybe CreateLobbyResponse of
        Nothing -> expectationFailure "Failed to decode create response"
        Just cr -> do
          let lid = clrLobbyId cr
          _ <- postJSON app (joinPath lid) (JoinLobbyRequest "Bob")
          resp <- postJSON app (startPath lid) ("null" :: String)
          simpleStatus resp `shouldBe` status200
          let mStart = decode (simpleBody resp) :: Maybe StartGameResponse
          mStart `shouldSatisfy` \case
            Just r  -> sgrGameId r /= ""
            Nothing -> False

    it "returns 400 with only 1 player" $ do
      (_, app) <- testAppWithState
      createResp <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "My Lobby")
      case decode (simpleBody createResp) :: Maybe CreateLobbyResponse of
        Nothing -> expectationFailure "Failed to decode create response"
        Just cr -> do
          let lid = clrLobbyId cr
          resp <- postJSON app (startPath lid) ("null" :: String)
          simpleStatus resp `shouldBe` status400

    it "returns 400 when already started" $ do
      (_, app) <- testAppWithState
      createResp <- postJSON app "/api/v1/lobbies" (CreateLobbyRequest "Alice" "My Lobby")
      case decode (simpleBody createResp) :: Maybe CreateLobbyResponse of
        Nothing -> expectationFailure "Failed to decode create response"
        Just cr -> do
          let lid = clrLobbyId cr
          _ <- postJSON app (joinPath lid) (JoinLobbyRequest "Bob")
          _ <- postJSON app (startPath lid) ("null" :: String)
          resp <- postJSON app (startPath lid) ("null" :: String)
          simpleStatus resp `shouldBe` status400

-- Helpers

joinPath :: LobbyId -> ByteString
joinPath lid = "/api/v1/lobbies/" <> TE.encodeUtf8 lid <> "/join"

startPath :: LobbyId -> ByteString
startPath lid = "/api/v1/lobbies/" <> TE.encodeUtf8 lid <> "/start"
