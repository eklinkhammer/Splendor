import { useState } from 'react';
import { joinLobby } from '../../services/api';
import { useSessionStore } from '../../stores/sessionStore';
import type { Lobby } from '../../types';

interface Props {
  lobby: Lobby;
  onJoined: (lobbyId: string) => void;
  onCancel: () => void;
}

export function JoinLobbyDialog({ lobby, onJoined, onCancel }: Props) {
  const [playerName, setPlayerName] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const setSession = useSessionStore((s) => s.setSession);
  const setLobbyId = useSessionStore((s) => s.setLobbyId);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!playerName.trim()) return;
    setLoading(true);
    setError(null);
    try {
      const res = await joinLobby(lobby.lobbyId, playerName.trim());
      setSession(res.jlrSessionId, playerName.trim());
      setLobbyId(lobby.lobbyId);
      onJoined(lobby.lobbyId);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to join lobby');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
      <div className="bg-gray-800 rounded-lg p-6 w-80 shadow-xl">
        <h3 className="text-lg font-semibold mb-1 text-gray-100">Join {lobby.lobbyName}</h3>
        <p className="text-gray-500 text-sm mb-4">
          {lobby.lobbySlots.length}/{lobby.lobbyMaxPlayers} players
        </p>
        <form onSubmit={handleSubmit} className="space-y-3">
          <input
            type="text"
            placeholder="Your name"
            value={playerName}
            onChange={(e) => setPlayerName(e.target.value)}
            className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-gray-100 placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-blue-400"
            maxLength={20}
            autoFocus
          />
          {error && <p className="text-red-500 text-sm">{error}</p>}
          <div className="flex gap-2">
            <button
              type="button"
              onClick={onCancel}
              className="flex-1 px-4 py-2 border border-gray-600 rounded text-gray-300 hover:bg-gray-700"
            >
              Cancel
            </button>
            <button
              type="submit"
              disabled={loading || !playerName.trim()}
              className="flex-1 px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {loading ? 'Joining...' : 'Join'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
