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

export interface ChatMsg {
  sender: string;
  message: string;
}

interface GameState {
  gameView: PublicGameView | null;
  previousGameView: PublicGameView | null;
  moveLog: MoveLogEntry[];
  lastMove: { playerId: PlayerId; description: string } | null;
  legalActions: Action[];
  gemReturnInfo: { amount: number; options: GemCollection[] } | null;
  nobleChoices: Noble[] | null;
  gameResult: GameResult | null;
  chatMessages: ChatMsg[];
  error: string | null;
  connected: boolean;
  selfPlayerId: PlayerId | null;

  // Hotseat multiplayer
  localPlayerIds: PlayerId[];
  activeSessionId: string | null;
  sessionPlayerMap: Record<string, PlayerId>;

  setConnected: (connected: boolean) => void;
  handleServerMessage: (sessionId: string, msg: ServerMessage) => void;
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
  chatMessages: [],
  error: null,
  connected: false,
  selfPlayerId: null,
  localPlayerIds: [],
  activeSessionId: null,
  sessionPlayerMap: {},

  setConnected: (connected) => set({ connected }),

  handleServerMessage: (sessionId, msg) => {
    switch (msg.tag) {
      case 'GameStateUpdate': {
        const view = msg.contents;
        const state = get();

        // Derive playerId for this session from ppReserved
        const selfPlayer = view.pgvPlayers.find((p) => p.ppReserved !== null);
        const sessionPlayerId = selfPlayer?.ppPlayerId ?? null;

        // Update session → player mapping
        const newMap = { ...state.sessionPlayerMap };
        if (sessionPlayerId) {
          newMap[sessionId] = sessionPlayerId;
        }

        // Collect all known local player IDs
        const newLocalPlayerIds = [...new Set(Object.values(newMap))];

        // Determine who the current turn player is
        const currentTurnPlayerId = view.pgvPlayers[view.pgvCurrentPlayer]?.ppPlayerId;
        const isLocalPlayerTurn = currentTurnPlayerId != null && newLocalPlayerIds.includes(currentTurnPlayerId);

        // Use this view if:
        // 1. It's this session's player's turn, OR
        // 2. No game view yet (first load)
        const useThisView = !state.gameView || (sessionPlayerId === currentTurnPlayerId);

        const prev = state.previousGameView;
        let newMoveLog = state.moveLog;
        let newLastMove = state.lastMove;

        if (useThisView && prev && view.pgvTurnNumber > prev.pgvTurnNumber) {
          const entry = computeMoveLogEntry(prev, view);
          if (entry) {
            newMoveLog = [...newMoveLog, entry];
            newLastMove = { playerId: entry.playerId, description: entry.description };
          }
        }

        if (useThisView) {
          set({
            gameView: view,
            previousGameView: (prev && view.pgvTurnNumber === prev.pgvTurnNumber)
              ? prev
              : view,
            moveLog: newMoveLog,
            lastMove: newLastMove,
            selfPlayerId: isLocalPlayerTurn ? currentTurnPlayerId : (sessionPlayerId ?? state.selfPlayerId),
            legalActions: [],
            gemReturnInfo: null,
            nobleChoices: null,
            error: null,
            sessionPlayerMap: newMap,
            localPlayerIds: newLocalPlayerIds,
          });
        } else {
          // Still update the mapping even if we don't switch the view
          set({
            sessionPlayerMap: newMap,
            localPlayerIds: newLocalPlayerIds,
          });
        }
        break;
      }
      case 'ActionRequired':
        set({ legalActions: msg.contents, activeSessionId: sessionId });
        break;
      case 'GemReturnNeeded':
        set({
          gemReturnInfo: { amount: msg.contents[0], options: msg.contents[1] },
          legalActions: [],
          activeSessionId: sessionId,
        });
        break;
      case 'NobleChoiceRequired':
        set({
          nobleChoices: msg.contents,
          legalActions: [],
          activeSessionId: sessionId,
        });
        break;
      case 'GameOverMsg':
        set({ gameResult: msg.contents, legalActions: [] });
        break;
      case 'ChatMessage':
        set({ chatMessages: [...get().chatMessages, { sender: msg.contents[0], message: msg.contents[1] }] });
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
      chatMessages: [],
      error: null,
      connected: false,
      selfPlayerId: null,
      localPlayerIds: [],
      activeSessionId: null,
      sessionPlayerMap: {},
    }),
}));
