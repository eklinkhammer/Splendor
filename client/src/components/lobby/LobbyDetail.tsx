import { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { getLobby, startGame } from '../../services/api';
import { useSessionStore } from '../../stores/sessionStore';
import type { Lobby } from '../../types';

interface Props {
  lobbyId: string;
}

export function LobbyDetail({ lobbyId }: Props) {
  const [lobby, setLobby] = useState<Lobby | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [starting, setStarting] = useState(false);
  const navigate = useNavigate();
  const sessionId = useSessionStore((s) => s.sessionId);
  const setGameId = useSessionStore((s) => s.setGameId);

  useEffect(() => {
    let active = true;
    const poll = async () => {
      try {
        const data = await getLobby(lobbyId);
        if (!active) return;
        setLobby(data);
        setError(null);
        // Navigate to game if started
        if (data.lobbyStatus.tag === 'Started') {
          const gameId = data.lobbyStatus.contents;
          setGameId(gameId);
          navigate(`/game/${gameId}`);
        }
      } catch (err) {
        if (active) setError(err instanceof Error ? err.message : 'Failed to load lobby');
      }
    };
    poll();
    const interval = setInterval(poll, 2000);
    return () => {
      active = false;
      clearInterval(interval);
    };
  }, [lobbyId, navigate, setGameId]);

  const handleStart = async () => {
    setStarting(true);
    setError(null);
    try {
      const res = await startGame(lobbyId);
      setGameId(res.sgrGameId);
      navigate(`/game/${res.sgrGameId}`);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to start game');
    } finally {
      setStarting(false);
    }
  };

  if (!lobby) {
    return <p className="text-gray-500">Loading lobby...</p>;
  }

  const isCreator = lobby.lobbySlots[0]?.lsSessionId === sessionId;
  const canStart = isCreator && lobby.lobbySlots.length >= lobby.lobbyMinPlayers;

  return (
    <div className="space-y-4">
      <h2 className="text-lg font-semibold">{lobby.lobbyName}</h2>
      <div>
        <h3 className="text-sm font-medium text-gray-600 mb-1">
          Players ({lobby.lobbySlots.length}/{lobby.lobbyMaxPlayers})
        </h3>
        <ul className="space-y-1">
          {lobby.lobbySlots.map((slot, i) => (
            <li key={slot.lsSessionId} className="flex items-center gap-2 p-2 bg-gray-50 rounded">
              <span className="w-6 h-6 flex items-center justify-center bg-blue-100 text-blue-700 text-xs rounded-full font-bold">
                {i + 1}
              </span>
              <span>{slot.lsPlayerName}</span>
              {slot.lsSessionId === sessionId && (
                <span className="text-xs text-blue-500">(you)</span>
              )}
            </li>
          ))}
        </ul>
      </div>
      {error && <p className="text-red-500 text-sm">{error}</p>}
      {lobby.lobbyStatus.tag === 'Waiting' && (
        <div>
          {canStart ? (
            <button
              onClick={handleStart}
              disabled={starting}
              className="w-full px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50"
            >
              {starting ? 'Starting...' : 'Start Game'}
            </button>
          ) : isCreator ? (
            <p className="text-gray-500 text-sm text-center">
              Waiting for more players ({lobby.lobbyMinPlayers} minimum)...
            </p>
          ) : (
            <p className="text-gray-500 text-sm text-center">Waiting for host to start...</p>
          )}
        </div>
      )}
    </div>
  );
}
