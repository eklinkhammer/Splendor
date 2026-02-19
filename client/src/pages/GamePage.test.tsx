// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, waitFor, cleanup } from '@testing-library/react';
import { useSessionStore } from '../stores/sessionStore';
import { useGameStore } from '../stores/gameStore';
import type { PublicPlayer, PublicGameView } from '../types';

// --- Configurable mock state ---
let mockPathname = '/game/game-1';
let mockSearchParams = new URLSearchParams('s=s1');
const mockNavigate = vi.fn();

vi.mock('react-router-dom', () => ({
  useParams: () => ({ id: 'game-1' }),
  useNavigate: () => mockNavigate,
  useLocation: () => ({ pathname: mockPathname }),
  useSearchParams: () => [mockSearchParams],
}));

// Mock useGameSocket — avoid real WebSocket connections
vi.mock('../hooks/useGameSocket', () => ({
  useGameSocket: () => ({ send: vi.fn(), connected: false }),
}));

// Mock getGame
const mockGetGame = vi.fn();
vi.mock('../services/api', () => ({
  getGame: (...args: unknown[]) => mockGetGame(...args),
}));

// Mock sub-panels — avoid complex child component rendering
vi.mock('../components/action-panel/GemReturnPanel', () => ({
  GemReturnPanel: () => null,
}));
vi.mock('../components/action-panel/NobleChoicePanel', () => ({
  NobleChoicePanel: () => null,
}));

// Mock ActionPanel hooks — avoid complex child components
vi.mock('../components/action-panel/ActionPanel', () => ({
  useActionCallbacks: () => ({
    selectedCardId: null,
    selectedDeckTier: null,
    selectedBankGems: [],
    clearSelection: vi.fn(),
    highlightCards: new Set(),
    reservableDeckTiers: new Set(),
    onCardClick: vi.fn(),
    onDeckClick: vi.fn(),
    onBankGemClick: vi.fn(),
    selectedBuyAction: null,
    selectedReserveAction: null,
    selectedDeckReserveAction: null,
    matchedTakeAction: null,
    handleBuy: vi.fn(),
    handleReserve: vi.fn(),
    handleDeckReserve: vi.fn(),
    handleTakeGems: vi.fn(),
    availableGemColors: new Set(),
    pendingAction: null,
    excessGems: 0,
    confirmPendingAction: vi.fn(),
    cancelPendingAction: vi.fn(),
  }),
}));

import { GamePage } from './GamePage';

function makePlayer(overrides?: Partial<PublicPlayer>): PublicPlayer {
  return {
    ppPlayerId: 'player-1',
    ppPlayerName: 'Alice',
    ppTokens: {},
    ppPurchased: [],
    ppReservedCount: 0,
    ppReserved: null,
    ppNobles: [],
    ppPrestige: 0,
    ppIsAI: false,
    ...overrides,
  };
}

function makeGameView(overrides?: Partial<PublicGameView>): PublicGameView {
  const emptyRow = { publicDeckCount: 0, publicDisplay: [] };
  return {
    pgvGameId: 'game-1',
    pgvBoard: {
      publicTier1: emptyRow,
      publicTier2: emptyRow,
      publicTier3: emptyRow,
      publicNobles: [],
      publicBank: {},
    },
    pgvPlayers: [makePlayer({ ppReserved: [] })],
    pgvCurrentPlayer: 0,
    pgvTurnNumber: 1,
    pgvPhase: { tag: 'InProgress' },
    pgvTurnPhase: { tag: 'AwaitingAction' },
    ...overrides,
  };
}

describe('GamePage', () => {
  afterEach(cleanup);

  beforeEach(() => {
    vi.clearAllMocks();
    mockPathname = '/game/game-1';
    mockSearchParams = new URLSearchParams('s=s1');
    useSessionStore.setState({
      sessionId: 's1',
      playerName: 'TestPlayer',
      gameId: 'game-1',
      lobbyId: 'lobby-1',
    });
    useGameStore.getState().reset();
    mockGetGame.mockReturnValue(new Promise(() => {}));
  });

  describe('cleanup on unmount', () => {
    it('resets gameStore on unmount', () => {
      useGameStore.setState({ connected: true });
      const { unmount } = render(<GamePage />);
      unmount();
      expect(useGameStore.getState().connected).toBe(false);
      expect(useGameStore.getState().gameView).toBeNull();
    });

    it('does not clear sessionId or playerName on unmount', () => {
      const { unmount } = render(<GamePage />);
      unmount();
      expect(useSessionStore.getState().sessionId).toBe('s1');
      expect(useSessionStore.getState().playerName).toBe('TestPlayer');
    });
  });

  describe('session from URL', () => {
    it('reads sessionId from ?s= search param', () => {
      mockSearchParams = new URLSearchParams('s=url-session');
      render(<GamePage />);
      // Should show connecting message (no gameView yet), meaning sessionId was accepted
      expect(screen.getByText('Connecting to game...')).toBeTruthy();
    });

    it('redirects to / when no ?s= param and not spectator', () => {
      mockSearchParams = new URLSearchParams();
      render(<GamePage />);
      expect(mockNavigate).toHaveBeenCalledWith('/');
    });

    it('renders null when no ?s= param and not spectator', () => {
      mockSearchParams = new URLSearchParams();
      const { container } = render(<GamePage />);
      expect(container.innerHTML).toBe('');
    });

    it('caches gameId in sessionStore when sessionId is present', () => {
      useSessionStore.setState({ gameId: null });
      mockSearchParams = new URLSearchParams('s=s1');
      render(<GamePage />);
      expect(useSessionStore.getState().gameId).toBe('game-1');
    });

    it('calls getGame with sessionId from URL as REST fallback', () => {
      mockSearchParams = new URLSearchParams('s=rest-session');
      render(<GamePage />);
      expect(mockGetGame).toHaveBeenCalledWith('game-1', 'rest-session');
    });

    it('feeds getGame result into gameStore', async () => {
      const view = makeGameView();
      mockGetGame.mockResolvedValue(view);
      mockSearchParams = new URLSearchParams('s=s1');
      render(<GamePage />);

      await waitFor(() => {
        expect(useGameStore.getState().gameView).not.toBeNull();
      });
    });
  });

  describe('spectator mode', () => {
    it('does not redirect when spectator and no ?s= param', () => {
      mockPathname = '/game/game-1/spectate';
      mockSearchParams = new URLSearchParams();
      render(<GamePage />);
      expect(mockNavigate).not.toHaveBeenCalled();
    });

    it('shows spectator connecting message', () => {
      mockPathname = '/game/game-1/spectate';
      mockSearchParams = new URLSearchParams();
      render(<GamePage />);
      expect(screen.getByText('Connecting as spectator...')).toBeTruthy();
    });

    it('does not call getGame for spectators', () => {
      mockPathname = '/game/game-1/spectate';
      mockSearchParams = new URLSearchParams();
      render(<GamePage />);
      expect(mockGetGame).not.toHaveBeenCalled();
    });
  });
});
