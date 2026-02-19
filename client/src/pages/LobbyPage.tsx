import { useState, useEffect } from 'react';
import { useParams, useSearchParams } from 'react-router-dom';
import { getLobby, joinLobby } from '../services/api';
import { useSessionStore } from '../stores/sessionStore';
import { LobbyDetail } from '../components/lobby/LobbyDetail';
import type { Lobby } from '../types';

export function LobbyPage() {
  const { lobbyId } = useParams<{ lobbyId: string }>();
  const [searchParams, setSearchParams] = useSearchParams();
  const sessionId = searchParams.get('s');

  const [lobby, setLobby] = useState<Lobby | null>(null);
  const [playerName, setPlayerName] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const setSession = useSessionStore((s) => s.setSession);
  const setLobbyId = useSessionStore((s) => s.setLobbyId);

  // Fetch lobby info for the join form header
  useEffect(() => {
    if (!lobbyId || sessionId) return;
    let active = true;
    getLobby(lobbyId).then((data) => {
      if (active) setLobby(data);
    }).catch((err) => {
      if (active) setError(err instanceof Error ? err.message : 'Failed to load lobby');
    });
    return () => { active = false; };
  }, [lobbyId, sessionId]);

  if (!lobbyId) return null;

  const handleJoin = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!playerName.trim()) return;
    setLoading(true);
    setError(null);
    try {
      const res = await joinLobby(lobbyId, playerName.trim());
      setSession(res.jlrSessionId, playerName.trim());
      setLobbyId(lobbyId);
      setSearchParams({ s: res.jlrSessionId });
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to join lobby');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-gray-900">
      <div className="max-w-xl mx-auto py-8 px-4">
        <h1 className="text-3xl font-bold text-center mb-8 text-gray-100">Splendor</h1>
        <div className="bg-gray-800 rounded-lg shadow-lg p-6">
          {sessionId ? (
            <LobbyDetail lobbyId={lobbyId} sessionId={sessionId} />
          ) : (
            <>
              {lobby && (
                <div className="mb-4">
                  <h2 className="text-lg font-semibold text-gray-100">{lobby.lobbyName}</h2>
                  <p className="text-gray-400 text-sm">
                    {lobby.lobbySlots.length}/{lobby.lobbyMaxPlayers} players
                  </p>
                </div>
              )}
              <form onSubmit={handleJoin} className="space-y-3">
                <h3 className="text-md font-semibold text-gray-200">Join this lobby</h3>
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
                <button
                  type="submit"
                  disabled={loading || !playerName.trim()}
                  className="w-full px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  {loading ? 'Joining...' : 'Join'}
                </button>
              </form>
            </>
          )}
        </div>
      </div>
    </div>
  );
}
