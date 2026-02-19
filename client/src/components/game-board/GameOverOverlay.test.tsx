// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, fireEvent, waitFor, cleanup } from '@testing-library/react';
import { useSessionStore } from '../../stores/sessionStore';
import { useGameStore } from '../../stores/gameStore';
import type { GameResult, PublicPlayer, PublicGameView } from '../../types';

// --- Configurable mock state ---
let mockSearchParams = new URLSearchParams('s=session-1');
const mockNavigate = vi.fn();

vi.mock('react-router-dom', () => ({
  useNavigate: () => mockNavigate,
  useSearchParams: () => [mockSearchParams],
}));

const mockPlayAgain = vi.fn();
vi.mock('../../services/api', () => ({
  playAgain: (...args: unknown[]) => mockPlayAgain(...args),
}));

import { GameOverOverlay } from './GameOverOverlay';

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

const defaultResult: GameResult = {
  winnerId: 'player-1',
  winnerName: 'Alice',
  finalPrestige: 15,
};

describe('GameOverOverlay', () => {
  afterEach(cleanup);

  beforeEach(() => {
    vi.resetAllMocks();
    mockSearchParams = new URLSearchParams('s=session-1');
    useSessionStore.setState({
      sessionId: 'session-1',
      playerName: 'Alice',
      lobbyId: null,
      gameId: 'game-1',
    });
    useGameStore.getState().reset();
    // Set up a gameView with self (ppReserved !== null) and one AI opponent
    useGameStore.setState({
      gameView: makeGameView({
        pgvPlayers: [
          makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Alice', ppPrestige: 15, ppReserved: [] }),
          makePlayer({ ppPlayerId: 'p2', ppPlayerName: 'Bot', ppPrestige: 8, ppReserved: null, ppIsAI: true }),
        ],
      }),
    });
    mockPlayAgain.mockResolvedValue({
      lobbyId: 'new-lobby',
      sessionId: 'new-session',
      gameId: 'new-game',
    });
  });

  it('shows winner name and game over text', () => {
    render(<GameOverOverlay result={defaultResult} />);
    expect(screen.getByText('Game Over!')).toBeTruthy();
    expect(screen.getByText('wins!')).toBeTruthy();
    // Winner name appears in both the header and the ranked list
    expect(screen.getAllByText('Alice').length).toBeGreaterThanOrEqual(1);
  });

  it('shows Play Again button when sessionId is in URL', () => {
    render(<GameOverOverlay result={defaultResult} />);
    expect(screen.getByText('Play Again')).toBeTruthy();
  });

  it('hides Play Again button when no sessionId (spectator)', () => {
    mockSearchParams = new URLSearchParams();
    render(<GameOverOverlay result={defaultResult} />);
    expect(screen.queryByText('Play Again')).toBeNull();
  });

  it('always shows Return to Lobby button', () => {
    render(<GameOverOverlay result={defaultResult} />);
    expect(screen.getByText('Return to Lobby')).toBeTruthy();
  });

  it('handlePlayAgain calls playAgain with correct AI count', async () => {
    render(<GameOverOverlay result={defaultResult} />);
    fireEvent.click(screen.getByText('Play Again'));

    await waitFor(() => {
      // Self is p1 (ppReserved !== null), so AI count = total - 1 = 1
      expect(mockPlayAgain).toHaveBeenCalledWith('Alice', "Alice's Game", 1);
    });
  });

  it('handlePlayAgain navigates to new game URL', async () => {
    render(<GameOverOverlay result={defaultResult} />);
    fireEvent.click(screen.getByText('Play Again'));

    await waitFor(() => {
      expect(mockNavigate).toHaveBeenCalledWith('/game/new-game?s=new-session');
    });
  });

  it('handlePlayAgain updates session store', async () => {
    render(<GameOverOverlay result={defaultResult} />);
    fireEvent.click(screen.getByText('Play Again'));

    await waitFor(() => {
      const state = useSessionStore.getState();
      expect(state.sessionId).toBe('new-session');
      expect(state.gameId).toBe('new-game');
    });
  });

  it('Return to Lobby clears session and navigates home', () => {
    render(<GameOverOverlay result={defaultResult} />);
    fireEvent.click(screen.getByText('Return to Lobby'));

    expect(mockNavigate).toHaveBeenCalledWith('/');
    const state = useSessionStore.getState();
    expect(state.sessionId).toBeNull();
    expect(state.playerName).toBeNull();
  });

  it('derives AI count correctly with multiple opponents', async () => {
    useGameStore.setState({
      gameView: makeGameView({
        pgvPlayers: [
          makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Alice', ppReserved: [] }),
          makePlayer({ ppPlayerId: 'p2', ppPlayerName: 'Bot1', ppReserved: null, ppIsAI: true }),
          makePlayer({ ppPlayerId: 'p3', ppPlayerName: 'Bot2', ppReserved: null, ppIsAI: true }),
          makePlayer({ ppPlayerId: 'p4', ppPlayerName: 'Bot3', ppReserved: null, ppIsAI: true }),
        ],
      }),
    });

    render(<GameOverOverlay result={defaultResult} />);
    fireEvent.click(screen.getByText('Play Again'));

    await waitFor(() => {
      expect(mockPlayAgain).toHaveBeenCalledWith('Alice', "Alice's Game", 3);
    });
  });

  it('shows Setting up... while playAgain is in progress', async () => {
    mockPlayAgain.mockReturnValue(new Promise(() => {})); // never resolves
    render(<GameOverOverlay result={defaultResult} />);
    fireEvent.click(screen.getByText('Play Again'));

    await waitFor(() => {
      expect(screen.getByText('Setting up...')).toBeTruthy();
    });
  });

  it('ranks players by prestige descending', () => {
    useGameStore.setState({
      gameView: makeGameView({
        pgvPlayers: [
          makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Alice', ppPrestige: 8, ppReserved: [] }),
          makePlayer({ ppPlayerId: 'p2', ppPlayerName: 'Bob', ppPrestige: 15, ppReserved: null }),
        ],
      }),
    });

    render(<GameOverOverlay result={defaultResult} />);
    // VP badges show prestige values; Bob (15) should come before Alice (8)
    const vpBadges = screen.getAllByText(/VP/);
    expect(vpBadges.length).toBe(2);
    expect(vpBadges[0]!.textContent).toContain('15');
    expect(vpBadges[1]!.textContent).toContain('8');
  });
});
