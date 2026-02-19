import { useState } from 'react';
import { createLobby } from '../../services/api';
import { useSessionStore } from '../../stores/sessionStore';

interface Props {
  onCreated: (lobbyId: string, sessionId: string) => void;
}

export function CreateLobbyForm({ onCreated }: Props) {
  const [playerName, setPlayerName] = useState('');
  const [lobbyName, setLobbyName] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const setSession = useSessionStore((s) => s.setSession);
  const setLobbyId = useSessionStore((s) => s.setLobbyId);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!playerName.trim() || !lobbyName.trim()) return;
    setLoading(true);
    setError(null);
    try {
      const res = await createLobby(playerName.trim(), lobbyName.trim());
      setSession(res.clrSessionId, playerName.trim());
      setLobbyId(res.clrLobbyId);
      onCreated(res.clrLobbyId, res.clrSessionId);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to create lobby');
    } finally {
      setLoading(false);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-3">
      <h2 className="text-lg font-semibold text-gray-100">Create a Game</h2>
      <input
        type="text"
        placeholder="Your name"
        value={playerName}
        onChange={(e) => setPlayerName(e.target.value)}
        className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-gray-100 placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-blue-400"
        maxLength={20}
      />
      <input
        type="text"
        placeholder="Lobby name"
        value={lobbyName}
        onChange={(e) => setLobbyName(e.target.value)}
        className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-gray-100 placeholder-gray-400 focus:outline-none focus:ring-2 focus:ring-blue-400"
        maxLength={30}
      />
      {error && <p className="text-red-500 text-sm">{error}</p>}
      <button
        type="submit"
        disabled={loading || !playerName.trim() || !lobbyName.trim()}
        className="w-full px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
      >
        {loading ? 'Creating...' : 'Create Lobby'}
      </button>
    </form>
  );
}
