module Splendor.Server.API.GameHTTPSpec (spec) where

import Data.Aeson (decode)
import Data.ByteString (ByteString, isInfixOf)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (status200, status403, status404)
import Network.Wai.Test (simpleBody, simpleHeaders, simpleStatus)
import Test.Hspec

import Splendor.Server.App (mkApp)
import Splendor.Server.TestHelpers
import Splendor.Server.Types

spec :: Spec
spec = describe "Game HTTP endpoints" $ do

  describe "GET /api/v1/games/:id?session=:sid" $ do
    it "returns 200 with PublicGameView for valid session" $ do
      (ss, gid, s1, _s2) <- setupGame
      let app = mkApp ss
      resp <- getJSON app (gamePath gid s1)
      simpleStatus resp `shouldBe` status200
      let mView = decode (simpleBody resp) :: Maybe PublicGameView
      case mView of
        Just view -> pgvGameId view `shouldBe` gid
        Nothing   -> expectationFailure "Failed to decode PublicGameView"

    it "returns application/json content type" $ do
      (ss, gid, s1, _s2) <- setupGame
      let app = mkApp ss
      resp <- getJSON app (gamePath gid s1)
      let headers = simpleHeaders resp
      case lookup "Content-Type" headers of
        Just ct -> ct `shouldSatisfy` ("application/json" `isInfixOf`)
        Nothing -> expectationFailure "No Content-Type header"

    it "returns 403 for invalid session" $ do
      (ss, gid, _s1, _s2) <- setupGame
      let app = mkApp ss
      resp <- getJSON app (gamePath gid "bad-session")
      simpleStatus resp `shouldBe` status403

    it "returns 404 for nonexistent game" $ do
      (ss, _gid, s1, _s2) <- setupGame
      let app = mkApp ss
      resp <- getJSON app (gamePath "nonexistent-game" s1)
      simpleStatus resp `shouldBe` status404

    it "response decodes as valid PublicGameView" $ do
      (ss, gid, s1, _s2) <- setupGame
      let app = mkApp ss
      resp <- getJSON app (gamePath gid s1)
      let mView = decode (simpleBody resp) :: Maybe PublicGameView
      mView `shouldSatisfy` \case
        Just _  -> True
        Nothing -> False

    it "two sessions see different reserved visibility" $ do
      (ss, gid, s1, s2) <- setupGame
      let app = mkApp ss
      resp1 <- getJSON app (gamePath gid s1)
      resp2 <- getJSON app (gamePath gid s2)
      case (decode (simpleBody resp1), decode (simpleBody resp2)) of
        (Just v1, Just v2) -> do
          let players1 = pgvPlayers (v1 :: PublicGameView)
              players2 = pgvPlayers (v2 :: PublicGameView)
          -- In v1 (s1's view), player 0 (self) has Just, player 1 has Nothing
          ppReserved (players1 !! 0) `shouldBe` Just []
          ppReserved (players1 !! 1) `shouldBe` Nothing
          -- In v2 (s2's view), player 0 has Nothing, player 1 (self) has Just
          ppReserved (players2 !! 0) `shouldBe` Nothing
          ppReserved (players2 !! 1) `shouldBe` Just []
        _ -> expectationFailure "Failed to decode views"

    it "response has correct player count" $ do
      (ss, gid, s1, _s2) <- setupGame
      let app = mkApp ss
      resp <- getJSON app (gamePath gid s1)
      case decode (simpleBody resp) :: Maybe PublicGameView of
        Just view -> length (pgvPlayers view) `shouldBe` 2
        Nothing   -> expectationFailure "Failed to decode PublicGameView"

    it "missing session query param returns non-200" $ do
      (ss, gid, _s1, _s2) <- setupGame
      let app = mkApp ss
      resp <- getJSON app ("/api/v1/games/" <> TE.encodeUtf8 gid)
      simpleStatus resp `shouldSatisfy` (/= status200)

-- Helpers

gamePath :: GameId -> SessionId -> ByteString
gamePath gid sid = "/api/v1/games/" <> TE.encodeUtf8 gid <> "?session=" <> TE.encodeUtf8 sid
