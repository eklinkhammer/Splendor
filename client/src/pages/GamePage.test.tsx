// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render } from '@testing-library/react';
import { useSessionStore } from '../stores/sessionStore';
import { useGameStore } from '../stores/gameStore';

// Mock react-router-dom
vi.mock('react-router-dom', () => ({
  useParams: () => ({ id: 'game-1' }),
  useNavigate: () => vi.fn(),
}));

// Mock useGameSocket — avoid real WebSocket connections
vi.mock('../hooks/useGameSocket', () => ({
  useGameSocket: () => ({ send: vi.fn(), connected: false }),
}));

// Mock getGame — never-resolving promise prevents side effects
vi.mock('../services/api', () => ({
  getGame: () => new Promise(() => {}),
}));

// Mock ActionPanel — it uses DOM refs and complex child components
vi.mock('../components/action-panel/ActionPanel', () => ({
  ActionPanel: () => null,
  useActionCallbacks: () => ({
    selectedCardId: null,
    setSelectedCardId: vi.fn(),
    highlightCards: new Set(),
    onCardClick: vi.fn(),
    onDeckClick: vi.fn(),
  }),
}));

import { GamePage } from './GamePage';

describe('GamePage cleanup on unmount', () => {
  beforeEach(() => {
    // Reset stores to a known state before each test
    useSessionStore.setState({
      sessionId: 's1',
      playerName: 'TestPlayer',
      gameId: 'game-1',
      lobbyId: 'lobby-1',
    });
    useGameStore.getState().reset();
  });

  it('clears gameId from sessionStore on unmount', () => {
    const { unmount } = render(<GamePage />);
    unmount();
    expect(useSessionStore.getState().gameId).toBeNull();
  });

  it('clears lobbyId from sessionStore on unmount', () => {
    const { unmount } = render(<GamePage />);
    unmount();
    expect(useSessionStore.getState().lobbyId).toBeNull();
  });

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
