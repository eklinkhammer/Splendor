// @vitest-environment jsdom
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, fireEvent, waitFor, cleanup } from '@testing-library/react';
import { useSessionStore } from '../stores/sessionStore';

// --- Mock state ---
let mockLobbyId: string | undefined = 'lobby-1';
let mockSearchParams = new URLSearchParams();
const mockSetSearchParams = vi.fn();

vi.mock('react-router-dom', () => ({
  useParams: () => ({ lobbyId: mockLobbyId }),
  useSearchParams: () => [mockSearchParams, mockSetSearchParams],
}));

const mockGetLobby = vi.fn();
const mockJoinLobby = vi.fn();
vi.mock('../services/api', () => ({
  getLobby: (...args: unknown[]) => mockGetLobby(...args),
  joinLobby: (...args: unknown[]) => mockJoinLobby(...args),
}));

vi.mock('../components/lobby/LobbyDetail', () => ({
  LobbyDetail: ({ lobbyId, sessionId }: { lobbyId: string; sessionId: string }) => (
    <div data-testid="lobby-detail">LobbyDetail:{lobbyId}:{sessionId}</div>
  ),
}));

import { LobbyPage } from './LobbyPage';

describe('LobbyPage', () => {
  afterEach(cleanup);

  beforeEach(() => {
    vi.clearAllMocks();
    mockLobbyId = 'lobby-1';
    mockSearchParams = new URLSearchParams();
    useSessionStore.setState({
      sessionId: null,
      playerName: null,
      lobbyId: null,
      gameId: null,
    });
    mockGetLobby.mockResolvedValue({
      lobbyId: 'lobby-1',
      lobbyName: 'Test Lobby',
      lobbySlots: [{ lsSessionId: 's1', lsPlayerName: 'Host', lsIsAI: false }],
      lobbyMaxPlayers: 4,
      lobbyMinPlayers: 2,
      lobbyStatus: { tag: 'Waiting' },
      lobbyCreatedAt: '2026-01-01T00:00:00Z',
    });
    mockJoinLobby.mockResolvedValue({ jlrSessionId: 'new-session-123' });
  });

  it('renders join form when no ?s= param is present', async () => {
    render(<LobbyPage />);
    expect(screen.getByPlaceholderText('Your name')).toBeTruthy();
    expect(screen.getByText('Join')).toBeTruthy();
  });

  it('shows lobby info from getLobby response', async () => {
    render(<LobbyPage />);
    await waitFor(() => {
      expect(screen.getByText('Test Lobby')).toBeTruthy();
    });
    expect(screen.getByText('1/4 players')).toBeTruthy();
  });

  it('disables submit when player name is empty/whitespace', () => {
    render(<LobbyPage />);
    const button = screen.getByText('Join');
    expect(button).toHaveProperty('disabled', true);

    fireEvent.change(screen.getByPlaceholderText('Your name'), { target: { value: '   ' } });
    expect(button).toHaveProperty('disabled', true);
  });

  it('calls joinLobby on form submit and updates URL params', async () => {
    render(<LobbyPage />);
    fireEvent.change(screen.getByPlaceholderText('Your name'), { target: { value: 'Alice' } });
    fireEvent.click(screen.getByText('Join'));

    await waitFor(() => {
      expect(mockJoinLobby).toHaveBeenCalledWith('lobby-1', 'Alice');
    });
    expect(mockSetSearchParams).toHaveBeenCalledWith({ s: 'new-session-123' });
  });

  it('sets session store after successful join', async () => {
    render(<LobbyPage />);
    fireEvent.change(screen.getByPlaceholderText('Your name'), { target: { value: 'Alice' } });
    fireEvent.click(screen.getByText('Join'));

    await waitFor(() => {
      const state = useSessionStore.getState();
      expect(state.sessionId).toBe('new-session-123');
      expect(state.playerName).toBe('Alice');
      expect(state.lobbyId).toBe('lobby-1');
    });
  });

  it('displays error when joinLobby fails', async () => {
    mockJoinLobby.mockRejectedValue(new Error('Lobby is full'));
    render(<LobbyPage />);
    fireEvent.change(screen.getByPlaceholderText('Your name'), { target: { value: 'Alice' } });
    fireEvent.click(screen.getByText('Join'));

    await waitFor(() => {
      expect(screen.getByText('Lobby is full')).toBeTruthy();
    });
  });

  it('displays error when getLobby fails', async () => {
    mockGetLobby.mockRejectedValue(new Error('Not found'));
    render(<LobbyPage />);

    await waitFor(() => {
      expect(screen.getByText('Not found')).toBeTruthy();
    });
  });

  it('shows LobbyDetail when ?s= param exists', () => {
    mockSearchParams = new URLSearchParams('s=session-abc');
    render(<LobbyPage />);
    const detail = screen.getByTestId('lobby-detail');
    expect(detail.textContent).toBe('LobbyDetail:lobby-1:session-abc');
  });

  it('does not fetch lobby when ?s= param exists', () => {
    mockSearchParams = new URLSearchParams('s=session-abc');
    render(<LobbyPage />);
    expect(mockGetLobby).not.toHaveBeenCalled();
  });

  it('returns null when lobbyId is undefined', () => {
    mockLobbyId = undefined;
    const { container } = render(<LobbyPage />);
    expect(container.innerHTML).toBe('');
  });

  it('trims player name before joining', async () => {
    render(<LobbyPage />);
    fireEvent.change(screen.getByPlaceholderText('Your name'), { target: { value: '  Bob  ' } });
    fireEvent.click(screen.getByText('Join'));

    await waitFor(() => {
      expect(mockJoinLobby).toHaveBeenCalledWith('lobby-1', 'Bob');
    });
  });
});
