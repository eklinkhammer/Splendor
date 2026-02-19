import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { listLobbies } from '../../services/api';
import type { Lobby } from '../../types';

export function LobbyList() {
  const [lobbies, setLobbies] = useState<Lobby[]>([]);
  const [error, setError] = useState<string | null>(null);
  const navigate = useNavigate();

  useEffect(() => {
    let active = true;
    const poll = async () => {
      try {
        const data = await listLobbies();
        if (active) {
          setLobbies(data.filter((l) => l.lobbyStatus.tag === 'Waiting'));
          setError(null);
        }
      } catch (err) {
        if (active) setError(err instanceof Error ? err.message : 'Failed to load lobbies');
      }
    };
    poll();
    const interval = setInterval(poll, 3000);
    return () => {
      active = false;
      clearInterval(interval);
    };
  }, []);

  return (
    <div>
      <h2 className="text-lg font-semibold mb-2 text-gray-100">Open Lobbies</h2>
      {error && <p className="text-red-500 text-sm mb-2">{error}</p>}
      {lobbies.length === 0 ? (
        <p className="text-gray-400 text-sm">No open lobbies. Create one above!</p>
      ) : (
        <ul className="space-y-2">
          {lobbies.map((lobby) => (
            <li
              key={lobby.lobbyId}
              className="flex items-center justify-between p-3 border border-gray-600 rounded hover:bg-gray-700"
            >
              <div>
                <span className="font-medium text-gray-100">{lobby.lobbyName}</span>
                <span className="text-gray-400 text-sm ml-2">
                  {lobby.lobbySlots.length}/{lobby.lobbyMaxPlayers} players
                </span>
              </div>
              <button
                onClick={() => navigate(`/lobby/${lobby.lobbyId}`)}
                className="px-3 py-1 bg-green-600 text-white text-sm rounded hover:bg-green-700"
              >
                Join
              </button>
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}
