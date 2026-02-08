import { create } from 'zustand';
import { persist } from 'zustand/middleware';

interface LocalSession {
  sessionId: string;
  playerName: string;
}

interface SessionState {
  sessionId: string | null;
  playerName: string | null;
  lobbyId: string | null;
  gameId: string | null;
  localSessions: LocalSession[];
  setSession: (sessionId: string, playerName: string) => void;
  setLobbyId: (lobbyId: string | null) => void;
  setGameId: (gameId: string | null) => void;
  addLocalSession: (sessionId: string, playerName: string) => void;
  clearLocalSessions: () => void;
  clear: () => void;
}

export const useSessionStore = create<SessionState>()(
  persist(
    (set, get) => ({
      sessionId: null,
      playerName: null,
      lobbyId: null,
      gameId: null,
      localSessions: [],
      setSession: (sessionId, playerName) => set({ sessionId, playerName }),
      setLobbyId: (lobbyId) => set({ lobbyId }),
      setGameId: (gameId) => set({ gameId }),
      addLocalSession: (sessionId, playerName) =>
        set({ localSessions: [...get().localSessions, { sessionId, playerName }] }),
      clearLocalSessions: () => set({ localSessions: [] }),
      clear: () => set({ sessionId: null, playerName: null, lobbyId: null, gameId: null, localSessions: [] }),
    }),
    { name: 'splendor-session' },
  ),
);
