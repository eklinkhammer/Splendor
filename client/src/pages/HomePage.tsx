import { useNavigate } from 'react-router-dom';
import { CreateLobbyForm } from '../components/lobby/CreateLobbyForm';
import { LobbyList } from '../components/lobby/LobbyList';
import { useSessionStore } from '../stores/sessionStore';

export function HomePage() {
  const navigate = useNavigate();
  const gameId = useSessionStore((s) => s.gameId);
  const sessionId = useSessionStore((s) => s.sessionId);

  return (
    <div className="min-h-screen bg-gray-900">
      <div className="max-w-xl mx-auto py-8 px-4">
        <h1 className="text-3xl font-bold text-center mb-8 text-gray-100">Splendor</h1>

        <div className="space-y-6">
          {gameId && sessionId && (
            <div className="bg-gray-800 rounded-lg shadow-lg p-4 text-center">
              <button
                onClick={() => navigate(`/game/${gameId}?s=${sessionId}`)}
                className="px-6 py-2 bg-green-600 text-white rounded hover:bg-green-700 font-semibold"
              >
                Resume Game
              </button>
            </div>
          )}
          <div className="bg-gray-800 rounded-lg shadow-lg p-6">
            <CreateLobbyForm onCreated={(lobbyId, sid) => navigate(`/lobby/${lobbyId}?s=${sid}`)} />
          </div>
          <div className="bg-gray-800 rounded-lg shadow-lg p-6">
            <LobbyList />
          </div>
        </div>
      </div>
    </div>
  );
}
