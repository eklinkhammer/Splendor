import { useEffect, useState } from 'react';
import { useNavigate, Link } from 'react-router-dom';
import { getLobby, startGame, addAI, leaveLobby, joinLobby } from '../../services/api';
import { useSessionStore } from '../../stores/sessionStore';
import type { Lobby } from '../../types';

interface Props {
  lobbyId: string;
}

export function LobbyDetail({ lobbyId }: Props) {
  const [lobby, setLobby] = useState<Lobby | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [starting, setStarting] = useState(false);
  const [addingAI, setAddingAI] = useState(false);
  const [leaving, setLeaving] = useState(false);
  const [addingLocal, setAddingLocal] = useState(false);
  const [localNameInput, setLocalNameInput] = useState('');
  const [showLocalInput, setShowLocalInput] = useState(false);
  const navigate = useNavigate();
  const sessionId = useSessionStore((s) => s.sessionId);
  const localSessions = useSessionStore((s) => s.localSessions);
  const addLocalSession = useSessionStore((s) => s.addLocalSession);
  const clearSession = useSessionStore((s) => s.clear);
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

  const handleAddAI = async () => {
    setAddingAI(true);
    setError(null);
    try {
      await addAI(lobbyId);
      // Re-fetch lobby to show the new slot
      const data = await getLobby(lobbyId);
      setLobby(data);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to add AI player');
    } finally {
      setAddingAI(false);
    }
  };

  const handleAddLocal = async () => {
    const name = localNameInput.trim();
    if (!name) return;
    setAddingLocal(true);
    setError(null);
    try {
      const res = await joinLobby(lobbyId, name);
      addLocalSession(res.jlrSessionId, name);
      setLocalNameInput('');
      setShowLocalInput(false);
      const data = await getLobby(lobbyId);
      setLobby(data);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to add local player');
    } finally {
      setAddingLocal(false);
    }
  };

  const handleLeave = async () => {
    if (!sessionId) return;
    setLeaving(true);
    setError(null);
    try {
      await leaveLobby(lobbyId, sessionId);
      clearSession();
      navigate('/');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to leave lobby');
    } finally {
      setLeaving(false);
    }
  };

  if (!lobby) {
    return <p className="text-gray-400">Loading lobby...</p>;
  }

  const localSessionIds = new Set(localSessions.map((s) => s.sessionId));
  const isCreator = lobby.lobbySlots[0]?.lsSessionId === sessionId;
  const canStart = isCreator && lobby.lobbySlots.length >= lobby.lobbyMinPlayers;
  const canAddMore = isCreator && lobby.lobbySlots.length < lobby.lobbyMaxPlayers;

  return (
    <div className="space-y-4">
      <h2 className="text-lg font-semibold text-gray-100">{lobby.lobbyName}</h2>
      <div>
        <h3 className="text-sm font-medium text-gray-400 mb-1">
          Players ({lobby.lobbySlots.length}/{lobby.lobbyMaxPlayers})
        </h3>
        <ul className="space-y-1">
          {lobby.lobbySlots.map((slot, i) => (
            <li key={slot.lsSessionId} className="flex items-center gap-2 p-2 bg-gray-700/50 rounded">
              <span className="w-6 h-6 flex items-center justify-center bg-blue-500/20 text-blue-300 text-xs rounded-full font-bold">
                {i + 1}
              </span>
              <span className="text-gray-200">{slot.lsPlayerName}</span>
              {slot.lsIsAI && (
                <span className="text-xs text-purple-400 font-medium">(AI)</span>
              )}
              {slot.lsSessionId === sessionId && (
                <span className="text-xs text-blue-400">(you)</span>
              )}
              {localSessionIds.has(slot.lsSessionId) && (
                <span className="text-xs text-teal-400 font-medium">(local)</span>
              )}
            </li>
          ))}
        </ul>
      </div>
      {error && <p className="text-red-500 text-sm">{error}</p>}
      {lobby.lobbyStatus.tag === 'Started' && (
        <Link
          to={`/game/${lobby.lobbyStatus.contents}/spectate`}
          className="block w-full px-4 py-2 bg-indigo-600 text-white text-center rounded hover:bg-indigo-700"
        >
          Spectate Game
        </Link>
      )}
      {lobby.lobbyStatus.tag === 'Waiting' && (
        <div className="space-y-2">
          {canAddMore && (
            <div className="flex gap-2">
              <button
                onClick={handleAddAI}
                disabled={addingAI}
                className="flex-1 px-4 py-2 bg-purple-600 text-white rounded hover:bg-purple-700 disabled:opacity-50"
              >
                {addingAI ? 'Adding...' : 'Add AI Player'}
              </button>
              <button
                onClick={() => setShowLocalInput(true)}
                disabled={addingLocal || showLocalInput}
                className="flex-1 px-4 py-2 bg-teal-600 text-white rounded hover:bg-teal-700 disabled:opacity-50"
              >
                Add Local Player
              </button>
            </div>
          )}
          {showLocalInput && (
            <form
              onSubmit={(e) => { e.preventDefault(); handleAddLocal(); }}
              className="flex gap-2"
            >
              <input
                type="text"
                value={localNameInput}
                onChange={(e) => setLocalNameInput(e.target.value)}
                placeholder="Player name"
                autoFocus
                className="flex-1 px-3 py-2 bg-gray-700 text-white rounded border border-gray-600 focus:border-teal-500 focus:outline-none"
              />
              <button
                type="submit"
                disabled={addingLocal || !localNameInput.trim()}
                className="px-4 py-2 bg-teal-600 text-white rounded hover:bg-teal-700 disabled:opacity-50"
              >
                {addingLocal ? 'Adding...' : 'Join'}
              </button>
              <button
                type="button"
                onClick={() => { setShowLocalInput(false); setLocalNameInput(''); }}
                className="px-3 py-2 bg-gray-600 text-white rounded hover:bg-gray-700"
              >
                Cancel
              </button>
            </form>
          )}
          {canStart ? (
            <button
              onClick={handleStart}
              disabled={starting}
              className="w-full px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50"
            >
              {starting ? 'Starting...' : 'Start Game'}
            </button>
          ) : isCreator ? (
            <p className="text-gray-400 text-sm text-center">
              Waiting for more players ({lobby.lobbyMinPlayers} minimum)...
            </p>
          ) : (
            <p className="text-gray-400 text-sm text-center">Waiting for host to start...</p>
          )}
          <button
            onClick={handleLeave}
            disabled={leaving}
            className="w-full px-4 py-2 bg-gray-600 text-white rounded hover:bg-gray-700 disabled:opacity-50"
          >
            {leaving ? 'Leaving...' : 'Leave Lobby'}
          </button>
        </div>
      )}
    </div>
  );
}
