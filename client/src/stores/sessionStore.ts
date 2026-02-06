import { create } from 'zustand';
import { persist } from 'zustand/middleware';

interface SessionState {
  sessionId: string | null;
  playerName: string | null;
  lobbyId: string | null;
  gameId: string | null;
  setSession: (sessionId: string, playerName: string) => void;
  setLobbyId: (lobbyId: string | null) => void;
  setGameId: (gameId: string | null) => void;
  clear: () => void;
}

export const useSessionStore = create<SessionState>()(
  persist(
    (set) => ({
      sessionId: null,
      playerName: null,
      lobbyId: null,
      gameId: null,
      setSession: (sessionId, playerName) => set({ sessionId, playerName }),
      setLobbyId: (lobbyId) => set({ lobbyId }),
      setGameId: (gameId) => set({ gameId }),
      clear: () => set({ sessionId: null, playerName: null, lobbyId: null, gameId: null }),
    }),
    { name: 'splendor-session' },
  ),
);
