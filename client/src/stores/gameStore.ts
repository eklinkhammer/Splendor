import { create } from 'zustand';
import type {
  PublicGameView,
  Action,
  GemCollection,
  Noble,
  GameResult,
  ServerMessage,
  PlayerId,
} from '../types';
import { computeMoveLogEntry } from '../utils/moveLog';
import type { MoveLogEntry } from '../utils/moveLog';

interface GameState {
  gameView: PublicGameView | null;
  previousGameView: PublicGameView | null;
  moveLog: MoveLogEntry[];
  lastMove: { playerId: PlayerId; description: string } | null;
  legalActions: Action[];
  gemReturnInfo: { amount: number; options: GemCollection[] } | null;
  nobleChoices: Noble[] | null;
  gameResult: GameResult | null;
  error: string | null;
  connected: boolean;
  selfPlayerId: PlayerId | null;

  setConnected: (connected: boolean) => void;
  handleServerMessage: (msg: ServerMessage) => void;
  clearError: () => void;
  reset: () => void;
}

export const useGameStore = create<GameState>()((set, get) => ({
  gameView: null,
  previousGameView: null,
  moveLog: [],
  lastMove: null,
  legalActions: [],
  gemReturnInfo: null,
  nobleChoices: null,
  gameResult: null,
  error: null,
  connected: false,
  selfPlayerId: null,

  setConnected: (connected) => set({ connected }),

  handleServerMessage: (msg) => {
    switch (msg.tag) {
      case 'GameStateUpdate': {
        const view = msg.contents;
        let selfId = get().selfPlayerId;
        // Derive self from the player whose ppReserved is not null
        if (!selfId) {
          const selfPlayer = view.pgvPlayers.find((p) => p.ppReserved !== null);
          if (selfPlayer) {
            selfId = selfPlayer.ppPlayerId;
          }
        }

        const prev = get().previousGameView;
        let newMoveLog = get().moveLog;
        let newLastMove = get().lastMove;

        if (prev && view.pgvTurnNumber > prev.pgvTurnNumber) {
          const entry = computeMoveLogEntry(prev, view);
          if (entry) {
            newMoveLog = [...newMoveLog, entry];
            newLastMove = { playerId: entry.playerId, description: entry.description };
          }
        }

        set({
          gameView: view,
          previousGameView: (prev && view.pgvTurnNumber === prev.pgvTurnNumber)
            ? prev   // keep start-of-turn snapshot
            : view,  // new turn started (or first snapshot)
          moveLog: newMoveLog,
          lastMove: newLastMove,
          selfPlayerId: selfId,
          legalActions: [],
          gemReturnInfo: null,
          nobleChoices: null,
          error: null,
        });
        break;
      }
      case 'ActionRequired':
        set({ legalActions: msg.contents });
        break;
      case 'GemReturnNeeded':
        set({
          gemReturnInfo: { amount: msg.contents[0], options: msg.contents[1] },
          legalActions: [],
        });
        break;
      case 'NobleChoiceRequired':
        set({
          nobleChoices: msg.contents,
          legalActions: [],
        });
        break;
      case 'GameOverMsg':
        set({ gameResult: msg.contents, legalActions: [] });
        break;
      case 'ErrorMsg':
        set({ error: msg.contents });
        break;
      case 'Pong':
        break;
    }
  },

  clearError: () => set({ error: null }),

  reset: () =>
    set({
      gameView: null,
      previousGameView: null,
      moveLog: [],
      lastMove: null,
      legalActions: [],
      gemReturnInfo: null,
      nobleChoices: null,
      gameResult: null,
      error: null,
      connected: false,
      selfPlayerId: null,
    }),
}));
