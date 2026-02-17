module Splendor.Server.WebSocketSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import System.Timeout (timeout)
import Test.Hspec

import Splendor.Core.Types
import Splendor.Core.Rules.ActionValidation (legalActions, legalGemReturns)
import Splendor.Server.App (mkApp)
import Splendor.Server.TestHelpers
import Splendor.Server.Types

-- | Receive and decode a ServerMessage with a 5-second timeout.
recvMsg :: WS.Connection -> IO ServerMessage
recvMsg conn = do
  mResult <- timeout 5000000 $ do
    raw <- WS.receiveData conn
    case decode raw of
      Just msg -> pure msg
      Nothing  -> error $ "Failed to decode ServerMessage: " ++ show (LBS.take 200 raw)
  case mResult of
    Just msg -> pure msg
    Nothing  -> error "Timed out waiting for WebSocket message"

-- | Helper: run a WS client against a test server with a game already set up.
withGameWS :: (Warp.Port -> ServerState -> GameId -> SessionId -> SessionId -> IO a) -> IO a
withGameWS action = do
  (ss, gid, s1, s2) <- setupGame
  let app = mkApp ss
  Warp.testWithApplication (pure app) $ \port ->
    action port ss gid s1 s2

-- | Connect with retry to handle the race between Warp.testWithApplication
--   starting the server and the TCP listen socket being ready.
connectWithRetry :: String -> Warp.Port -> String -> (WS.Connection -> IO a) -> IO a
connectWithRetry host port path action = go (3 :: Int)
  where
    go 0 = WS.runClient host port path action  -- last attempt, let exception propagate
    go n = do
      result <- try $ WS.runClient host port path action
      case result of
        Right a -> pure a
        Left (_ :: WS.HandshakeException) -> do
          threadDelay 50000  -- 50ms
          go (n - 1)

-- | Build the WS path for a game.
wsPath :: GameId -> SessionId -> String
wsPath gid sid = "/api/v1/games/" <> T.unpack gid <> "/ws?session=" <> T.unpack sid

-- | Retry an action up to @n@ extra times on ConnectionException (transient CI race).
retryOnConnectionClosed :: Int -> IO a -> IO a
retryOnConnectionClosed 0 action = action
retryOnConnectionClosed n action = do
  result <- try action
  case result of
    Right a -> pure a
    Left (_ :: WS.ConnectionException) -> do
      threadDelay 100000  -- 100ms
      retryOnConnectionClosed (n - 1) action

spec :: Spec
spec = describe "WebSocket protocol" $ do

  it "receives GameStateUpdate on connect" $
    retryOnConnectionClosed 2 $ withGameWS $ \port _ss gid s1 _s2 ->
      connectWithRetry "127.0.0.1" port (wsPath gid s1) $ \conn -> do
        msg <- recvMsg conn
        case msg of
          GameStateUpdate _ -> pure ()
          other -> expectationFailure $ "Expected GameStateUpdate, got: " ++ msgTag other

  it "current player receives ActionRequired after GameStateUpdate" $
    retryOnConnectionClosed 2 $ withGameWS $ \port ss gid s1 s2 -> do
      curSid <- currentSession ss gid s1 s2
      connectWithRetry "127.0.0.1" port (wsPath gid curSid) $ \conn -> do
        msg1 <- recvMsg conn
        case msg1 of
          GameStateUpdate _ -> pure ()
          other -> expectationFailure $ "Expected GameStateUpdate first, got: " ++ msgTag other
        msg2 <- recvMsg conn
        case msg2 of
          ActionRequired _ -> pure ()
          other -> expectationFailure $ "Expected ActionRequired, got: " ++ msgTag other

  it "non-current player receives only GameStateUpdate" $
    retryOnConnectionClosed 2 $ withGameWS $ \port ss gid s1 s2 -> do
      nonCurSid <- wrongSession ss gid s1 s2
      connectWithRetry "127.0.0.1" port (wsPath gid nonCurSid) $ \conn -> do
        msg1 <- recvMsg conn
        case msg1 of
          GameStateUpdate _ -> pure ()
          other -> expectationFailure $ "Expected GameStateUpdate, got: " ++ msgTag other
        -- Should not receive ActionRequired
        mMsg <- timeout 500000 (recvMsg conn)
        case mMsg of
          Nothing -> pure ()  -- Good, no extra message
          Just m  -> expectationFailure $ "Expected no more messages, got: " ++ msgTag m

  it "Ping returns Pong" $
    retryOnConnectionClosed 2 $ withGameWS $ \port _ss gid s1 _s2 ->
      connectWithRetry "127.0.0.1" port (wsPath gid s1) $ \conn -> do
        _ <- recvMsg conn  -- consume initial GameStateUpdate
        -- Drain any ActionRequired if this is the current player
        _ <- timeout 500000 (recvMsg conn)
        WS.sendTextData conn (encode Ping)
        msg <- recvMsg conn
        case msg of
          Pong -> pure ()
          other -> expectationFailure $ "Expected Pong, got: " ++ msgTag other

  it "valid TakeGems action triggers GameStateUpdate broadcast" $
    retryOnConnectionClosed 3 $ withGameWS $ \port ss gid s1 s2 -> do
      curSid <- currentSession ss gid s1 s2
      let otherSid = if curSid == s1 then s2 else s1
      connectWithRetry "127.0.0.1" port (wsPath gid curSid) $ \conn1 ->
        connectWithRetry "127.0.0.1" port (wsPath gid otherSid) $ \conn2 -> do
          -- Drain initial messages for both
          _ <- recvMsg conn1  -- GameStateUpdate
          _ <- timeout 500000 (recvMsg conn1)  -- possibly ActionRequired
          _ <- recvMsg conn2  -- GameStateUpdate
          _ <- timeout 500000 (recvMsg conn2)  -- possibly ActionRequired
          -- Get legal actions
          mg <- lookupGameOrFail ss gid
          let gs = mgGameState mg
              actions = legalActions gs
              gemAction = case [ a | a@(TakeGems _) <- actions ] of
                (a:_) -> a
                []    -> error "No gem-take actions available"
          -- Send action
          WS.sendTextData conn1 (encode (SubmitAction gemAction))
          -- Both should receive GameStateUpdate
          msg1 <- recvMsg conn1
          msg2 <- recvMsg conn2
          case msg1 of
            GameStateUpdate _ -> pure ()
            other -> expectationFailure $ "P1 expected GameStateUpdate, got: " ++ msgTag other
          case msg2 of
            GameStateUpdate _ -> pure ()
            other -> expectationFailure $ "P2 expected GameStateUpdate, got: " ++ msgTag other

  it "action from wrong player returns ErrorMsg" $
    retryOnConnectionClosed 2 $ withGameWS $ \port ss gid s1 s2 -> do
      nonCurSid <- wrongSession ss gid s1 s2
      connectWithRetry "127.0.0.1" port (wsPath gid nonCurSid) $ \conn -> do
        _ <- recvMsg conn  -- GameStateUpdate
        _ <- timeout 500000 (recvMsg conn)
        -- Send an action as the wrong player
        let action = SubmitAction (TakeGems (TakeDifferent [Ruby, Diamond, Emerald]))
        WS.sendTextData conn (encode action)
        msg <- recvMsg conn
        case msg of
          ErrorMsg _ -> pure ()
          other -> expectationFailure $ "Expected ErrorMsg, got: " ++ msgTag other

  it "malformed JSON returns ErrorMsg" $
    retryOnConnectionClosed 2 $ withGameWS $ \port _ss gid s1 _s2 ->
      connectWithRetry "127.0.0.1" port (wsPath gid s1) $ \conn -> do
        _ <- recvMsg conn  -- GameStateUpdate
        _ <- timeout 500000 (recvMsg conn)
        WS.sendTextData conn ("{invalid json" :: LBS.ByteString)
        msg <- recvMsg conn
        case msg of
          ErrorMsg _ -> pure ()
          other -> expectationFailure $ "Expected ErrorMsg, got: " ++ msgTag other

  it "invalid session is rejected" $ do
    (ss, gid, _s1, _s2) <- setupGame
    let app = mkApp ss
    Warp.testWithApplication (pure app) $ \port -> do
      result <- try $
        WS.runClient "127.0.0.1" port (wsPath gid "invalid-session") $ \conn -> do
          _ <- recvMsg conn
          pure ()
      case result of
        Left (_ :: WS.HandshakeException) -> pure ()  -- Connection rejected as expected
        Right () -> expectationFailure "Expected connection to be rejected, but it was accepted"

  it "invalid gameId is rejected" $ do
    (ss, _gid, s1, _s2) <- setupGame
    let app = mkApp ss
    Warp.testWithApplication (pure app) $ \port -> do
      result <- try $
        WS.runClient "127.0.0.1" port (wsPath "nonexistent-game" s1) $ \conn -> do
          _ <- recvMsg conn
          pure ()
      case result of
        Left (_ :: WS.HandshakeException) -> pure ()  -- Connection rejected as expected
        Right () -> expectationFailure "Expected connection to be rejected, but it was accepted"

  it "two clients: action triggers cross-client broadcast" $
    retryOnConnectionClosed 3 $ withGameWS $ \port ss gid s1 s2 -> do
      curSid <- currentSession ss gid s1 s2
      let otherSid = if curSid == s1 then s2 else s1
      connectWithRetry "127.0.0.1" port (wsPath gid curSid) $ \conn1 ->
        connectWithRetry "127.0.0.1" port (wsPath gid otherSid) $ \conn2 -> do
          -- Drain initial messages
          _ <- recvMsg conn1
          _ <- timeout 500000 (recvMsg conn1)
          _ <- recvMsg conn2
          _ <- timeout 500000 (recvMsg conn2)
          -- Get a valid action
          mg <- lookupGameOrFail ss gid
          let gs = mgGameState mg
              actions = legalActions gs
              action = case actions of
                (a:_) -> a
                []    -> error "No actions available"
          WS.sendTextData conn1 (encode (SubmitAction action))
          -- Other player should receive the update
          msg2 <- recvMsg conn2
          case msg2 of
            GameStateUpdate _ -> pure ()
            other -> expectationFailure $ "Expected cross-client GameStateUpdate, got: " ++ msgTag other

  it "disconnect and reconnect yields fresh state" $
    retryOnConnectionClosed 2 $ withGameWS $ \port _ss gid s1 _s2 -> do
      -- First connection
      connectWithRetry "127.0.0.1" port (wsPath gid s1) $ \conn -> do
        msg <- recvMsg conn
        case msg of
          GameStateUpdate _ -> pure ()
          other -> expectationFailure $ "Expected GameStateUpdate, got: " ++ msgTag other
      -- Reconnect with retries — server cleanup from the first connection
      -- may race with the second connection's setup.
      let tryReconnect :: Int -> IO ()
          tryReconnect 0 = expectationFailure "Could not reconnect after retries"
          tryReconnect n = do
            threadDelay 500000  -- 500ms
            result <- try $ WS.runClient "127.0.0.1" port (wsPath gid s1) $ \conn -> do
              msg <- recvMsg conn
              case msg of
                GameStateUpdate _ -> pure ()
                other -> expectationFailure $ "Expected GameStateUpdate on reconnect, got: " ++ msgTag other
            case result of
              Right () -> pure ()
              Left (_ :: WS.ConnectionException) -> tryReconnect (n - 1)
      tryReconnect 5

  it "full game via WS completes with GameOverMsg" $
    retryOnConnectionClosed 2 $ withGameWS $ \port ss gid s1 s2 -> do
      connectWithRetry "127.0.0.1" port (wsPath gid s1) $ \conn1 ->
        connectWithRetry "127.0.0.1" port (wsPath gid s2) $ \conn2 -> do
          -- Drain initial messages
          _ <- recvMsg conn1
          _ <- timeout 500000 (recvMsg conn1)
          _ <- recvMsg conn2
          _ <- timeout 500000 (recvMsg conn2)
          -- Play turns until game over (max 500 iterations to prevent infinite loop)
          playUntilGameOver ss gid s1 s2 conn1 conn2 500

-- | Play random legal actions until GameOver or iteration limit.
playUntilGameOver :: ServerState -> GameId -> SessionId -> SessionId
                  -> WS.Connection -> WS.Connection -> Int -> IO ()
playUntilGameOver _ss _gid _s1 _s2 _conn1 _conn2 0 =
  expectationFailure "Game did not finish within iteration limit"
playUntilGameOver ss gid s1 s2 conn1 conn2 remaining = do
  -- Small delay to let server finish processing
  threadDelay 50000
  mg <- lookupGameOrFail ss gid
  let gs = mgGameState mg
  case gsPhase gs of
    Finished _ -> do
      -- Drain any remaining messages
      drainWS conn1
      drainWS conn2
      pure ()  -- Game is done
    _ -> do
      curSid <- currentSession ss gid s1 s2
      let curConn = if curSid == s1 then conn1 else conn2
      case gsTurnPhase gs of
        AwaitingAction -> do
          -- Check if there are pending nobles to choose
          case mgPendingNobles mg of
            Just (noble:_) -> do
              WS.sendTextData curConn (encode (ChooseNoble (nobleId noble)))
              drainWS conn1
              drainWS conn2
              playUntilGameOver ss gid s1 s2 conn1 conn2 (remaining - 1)
            _ -> do
              let actions = legalActions gs
              case actions of
                [] -> do
                  -- Game may have ended between our read and now
                  mg' <- lookupGameOrFail ss gid
                  case gsPhase (mgGameState mg') of
                    Finished _ -> do
                      drainWS conn1
                      drainWS conn2
                      pure ()
                    _ -> expectationFailure "No legal actions available"
                (action:_) -> do
                  WS.sendTextData curConn (encode (SubmitAction action))
                  -- Drain messages from both connections
                  drainWS conn1
                  drainWS conn2
                  playUntilGameOver ss gid s1 s2 conn1 conn2 (remaining - 1)
        MustReturnGems _ -> do
          let returns = legalGemReturns gs
          case returns of
            [] -> expectationFailure "No legal gem returns"
            (ret:_) -> do
              WS.sendTextData curConn (encode (ReturnGems ret))
              drainWS conn1
              drainWS conn2
              playUntilGameOver ss gid s1 s2 conn1 conn2 (remaining - 1)

-- | Drain available WS messages (non-blocking) to avoid message backup.
drainWS :: WS.Connection -> IO ()
drainWS conn = do
  mMsg <- timeout 50000 (WS.receiveData conn :: IO LBS.ByteString)
  case mMsg of
    Just _ -> drainWS conn  -- Keep draining
    Nothing -> pure ()
