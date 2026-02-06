import { describe, it, expect, beforeEach } from 'vitest';
import { useSessionStore } from './sessionStore';

beforeEach(() => {
  useSessionStore.setState({
    sessionId: null,
    playerName: null,
    lobbyId: null,
    gameId: null,
  });
});

describe('sessionStore', () => {
  describe('initial state', () => {
    it('starts with all null values', () => {
      const state = useSessionStore.getState();
      expect(state.sessionId).toBeNull();
      expect(state.playerName).toBeNull();
      expect(state.lobbyId).toBeNull();
      expect(state.gameId).toBeNull();
    });
  });

  describe('setSession', () => {
    it('sets sessionId and playerName', () => {
      useSessionStore.getState().setSession('sess-123', 'Alice');
      const state = useSessionStore.getState();
      expect(state.sessionId).toBe('sess-123');
      expect(state.playerName).toBe('Alice');
    });

    it('overwrites previous session', () => {
      useSessionStore.getState().setSession('sess-1', 'Alice');
      useSessionStore.getState().setSession('sess-2', 'Bob');
      const state = useSessionStore.getState();
      expect(state.sessionId).toBe('sess-2');
      expect(state.playerName).toBe('Bob');
    });
  });

  describe('setLobbyId', () => {
    it('sets lobbyId', () => {
      useSessionStore.getState().setLobbyId('lobby-42');
      expect(useSessionStore.getState().lobbyId).toBe('lobby-42');
    });

    it('clears lobbyId when passed null', () => {
      useSessionStore.getState().setLobbyId('lobby-42');
      useSessionStore.getState().setLobbyId(null);
      expect(useSessionStore.getState().lobbyId).toBeNull();
    });
  });

  describe('setGameId', () => {
    it('sets gameId', () => {
      useSessionStore.getState().setGameId('game-99');
      expect(useSessionStore.getState().gameId).toBe('game-99');
    });

    it('clears gameId when passed null', () => {
      useSessionStore.getState().setGameId('game-99');
      useSessionStore.getState().setGameId(null);
      expect(useSessionStore.getState().gameId).toBeNull();
    });
  });

  describe('clear', () => {
    it('resets all fields to null', () => {
      useSessionStore.getState().setSession('sess-1', 'Bob');
      useSessionStore.getState().setLobbyId('lobby-1');
      useSessionStore.getState().setGameId('game-1');
      useSessionStore.getState().clear();
      const state = useSessionStore.getState();
      expect(state.sessionId).toBeNull();
      expect(state.playerName).toBeNull();
      expect(state.lobbyId).toBeNull();
      expect(state.gameId).toBeNull();
    });
  });

  describe('independence', () => {
    it('setSession does not clear lobbyId or gameId', () => {
      const { setLobbyId, setGameId, setSession } = useSessionStore.getState();
      setLobbyId('lobby-x');
      setGameId('game-x');
      setSession('sess-new', 'Charlie');
      const state = useSessionStore.getState();
      expect(state.sessionId).toBe('sess-new');
      expect(state.lobbyId).toBe('lobby-x');
      expect(state.gameId).toBe('game-x');
    });

    it('setLobbyId does not affect session or gameId', () => {
      const { setSession, setGameId, setLobbyId } = useSessionStore.getState();
      setSession('sess-1', 'Alice');
      setGameId('game-1');
      setLobbyId('lobby-new');
      const state = useSessionStore.getState();
      expect(state.sessionId).toBe('sess-1');
      expect(state.gameId).toBe('game-1');
      expect(state.lobbyId).toBe('lobby-new');
    });
  });
});
