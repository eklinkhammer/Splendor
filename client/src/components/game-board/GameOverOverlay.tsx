import { useNavigate } from 'react-router-dom';
import type { GameResult } from '../../types';
import { useSessionStore } from '../../stores/sessionStore';
import { useGameStore } from '../../stores/gameStore';

interface Props {
  result: GameResult;
}

export function GameOverOverlay({ result }: Props) {
  const navigate = useNavigate();
  const clearSession = useSessionStore((s) => s.clear);
  const resetGame = useGameStore((s) => s.reset);

  const handleReturn = () => {
    resetGame();
    clearSession();
    navigate('/');
  };

  return (
    <div className="fixed inset-0 bg-black/60 flex items-center justify-center z-50">
      <div className="bg-white rounded-xl p-8 shadow-2xl text-center max-w-sm">
        <h2 className="text-2xl font-bold mb-2">Game Over!</h2>
        <p className="text-lg mb-1">
          <span className="font-semibold text-purple-700">{result.winnerName}</span> wins!
        </p>
        <p className="text-gray-600 mb-6">{result.finalPrestige} prestige points</p>
        <button
          onClick={handleReturn}
          className="px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
        >
          Return to Lobby
        </button>
      </div>
    </div>
  );
}
