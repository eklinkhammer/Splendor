// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, fireEvent, cleanup } from '@testing-library/react';
import { useSessionStore } from '../stores/sessionStore';

const mockNavigate = vi.fn();

vi.mock('react-router-dom', () => ({
  useNavigate: () => mockNavigate,
}));

// Capture onCreated callback from CreateLobbyForm
let capturedOnCreated: ((lobbyId: string, sid: string) => void) | undefined;
vi.mock('../components/lobby/CreateLobbyForm', () => ({
  CreateLobbyForm: ({ onCreated }: { onCreated: (lobbyId: string, sid: string) => void }) => {
    capturedOnCreated = onCreated;
    return <div data-testid="create-lobby-form">CreateLobbyForm</div>;
  },
}));

vi.mock('../components/lobby/LobbyList', () => ({
  LobbyList: () => <div data-testid="lobby-list">LobbyList</div>,
}));

import { HomePage } from './HomePage';

describe('HomePage', () => {
  afterEach(cleanup);

  beforeEach(() => {
    vi.clearAllMocks();
    capturedOnCreated = undefined;
    useSessionStore.setState({
      sessionId: null,
      playerName: null,
      lobbyId: null,
      gameId: null,
    });
  });

  it('does not show Resume Game when no gameId or sessionId', () => {
    render(<HomePage />);
    expect(screen.queryByText('Resume Game')).toBeNull();
  });

  it('does not show Resume Game when only gameId is set', () => {
    useSessionStore.setState({ gameId: 'game-1' });
    render(<HomePage />);
    expect(screen.queryByText('Resume Game')).toBeNull();
  });

  it('does not show Resume Game when only sessionId is set', () => {
    useSessionStore.setState({ sessionId: 'sess-1' });
    render(<HomePage />);
    expect(screen.queryByText('Resume Game')).toBeNull();
  });

  it('shows Resume Game when both gameId and sessionId are set', () => {
    useSessionStore.setState({ gameId: 'game-1', sessionId: 'sess-1' });
    render(<HomePage />);
    expect(screen.getByText('Resume Game')).toBeTruthy();
  });

  it('Resume Game navigates to /game/:gameId?s=:sessionId', () => {
    useSessionStore.setState({ gameId: 'game-42', sessionId: 'sess-abc' });
    render(<HomePage />);
    fireEvent.click(screen.getByText('Resume Game'));
    expect(mockNavigate).toHaveBeenCalledWith('/game/game-42?s=sess-abc');
  });

  it('renders CreateLobbyForm and LobbyList', () => {
    render(<HomePage />);
    expect(screen.getByTestId('create-lobby-form')).toBeTruthy();
    expect(screen.getByTestId('lobby-list')).toBeTruthy();
  });

  it('CreateLobbyForm onCreated navigates to /lobby/:id?s=:sid', () => {
    render(<HomePage />);
    expect(capturedOnCreated).toBeDefined();
    capturedOnCreated!('lobby-99', 'sid-xyz');
    expect(mockNavigate).toHaveBeenCalledWith('/lobby/lobby-99?s=sid-xyz');
  });
});
