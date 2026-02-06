import type {
  Lobby,
  LobbySlot,
  CreateLobbyResponse,
  JoinLobbyResponse,
  StartGameResponse,
  PublicGameView,
} from '../types';

const BASE = '/api/v1';

async function request<T>(url: string, options?: RequestInit): Promise<T> {
  const res = await fetch(url, {
    headers: { 'Content-Type': 'application/json' },
    ...options,
  });
  if (!res.ok) {
    const text = await res.text();
    throw new Error(`${res.status}: ${text}`);
  }
  return res.json() as Promise<T>;
}

export function createLobby(playerName: string, lobbyName: string): Promise<CreateLobbyResponse> {
  return request(`${BASE}/lobbies`, {
    method: 'POST',
    body: JSON.stringify({ clrPlayerName: playerName, clrLobbyName: lobbyName }),
  });
}

export function listLobbies(): Promise<Lobby[]> {
  return request(`${BASE}/lobbies`);
}

export function getLobby(id: string): Promise<Lobby> {
  return request(`${BASE}/lobbies/${id}`);
}

export function joinLobby(id: string, playerName: string): Promise<JoinLobbyResponse> {
  return request(`${BASE}/lobbies/${id}/join`, {
    method: 'POST',
    body: JSON.stringify({ jlrPlayerName: playerName }),
  });
}

export function addAI(lobbyId: string): Promise<LobbySlot> {
  return request(`${BASE}/lobbies/${lobbyId}/add-ai`, { method: 'POST' });
}

export function startGame(lobbyId: string): Promise<StartGameResponse> {
  return request(`${BASE}/lobbies/${lobbyId}/start`, {
    method: 'POST',
  });
}

export function getGame(gameId: string, sessionId: string): Promise<PublicGameView> {
  return request(`${BASE}/games/${gameId}?session=${encodeURIComponent(sessionId)}`);
}
