import { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { CreateLobbyForm } from '../components/lobby/CreateLobbyForm';
import { LobbyList } from '../components/lobby/LobbyList';
import { LobbyDetail } from '../components/lobby/LobbyDetail';
import { JoinLobbyDialog } from '../components/lobby/JoinLobbyDialog';
import { useSessionStore } from '../stores/sessionStore';
import type { Lobby } from '../types';

export function HomePage() {
  const [activeLobbyId, setActiveLobbyId] = useState<string | null>(null);
  const [joiningLobby, setJoiningLobby] = useState<Lobby | null>(null);
  const navigate = useNavigate();
  const sessionId = useSessionStore((s) => s.sessionId);
  const lobbyId = useSessionStore((s) => s.lobbyId);
  const gameId = useSessionStore((s) => s.gameId);

  // Redirect to active game if we have one
  useEffect(() => {
    if (gameId && sessionId) {
      navigate(`/game/${gameId}`);
    }
  }, [gameId, sessionId, navigate]);

  // Restore active lobby from session
  useEffect(() => {
    if (lobbyId && sessionId && !activeLobbyId) {
      setActiveLobbyId(lobbyId);
    }
  }, [lobbyId, sessionId, activeLobbyId]);

  return (
    <div className="min-h-screen bg-gray-900">
      <div className="max-w-xl mx-auto py-8 px-4">
        <h1 className="text-3xl font-bold text-center mb-8 text-gray-100">Splendor</h1>

        {activeLobbyId ? (
          <div className="bg-gray-800 rounded-lg shadow-lg p-6">
            <LobbyDetail lobbyId={activeLobbyId} />
          </div>
        ) : (
          <div className="space-y-6">
            <div className="bg-gray-800 rounded-lg shadow-lg p-6">
              <CreateLobbyForm onCreated={setActiveLobbyId} />
            </div>
            <div className="bg-gray-800 rounded-lg shadow-lg p-6">
              <LobbyList
                onSelect={(lobby) => setJoiningLobby(lobby)}
              />
            </div>
          </div>
        )}
      </div>

      {joiningLobby && (
        <JoinLobbyDialog
          lobby={joiningLobby}
          onJoined={(id) => {
            setJoiningLobby(null);
            setActiveLobbyId(id);
          }}
          onCancel={() => setJoiningLobby(null)}
        />
      )}
    </div>
  );
}
