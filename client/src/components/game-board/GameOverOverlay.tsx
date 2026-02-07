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
      <div className="bg-gradient-to-b from-gray-800 to-gray-900 rounded-2xl p-10 shadow-2xl text-center max-w-sm border border-amber-500/30">
        <div className="text-5xl mb-3">🏆</div>
        <h2 className="text-2xl font-extrabold text-gray-100 mb-2">Game Over!</h2>
        <p className="text-xl mb-1 text-gray-200">
          <span className="font-bold bg-gradient-to-r from-purple-400 to-purple-600 bg-clip-text text-transparent">
            {result.winnerName}
          </span>{' '}
          wins!
        </p>
        <p className="mb-8">
          <span className="inline-flex items-center gap-1 bg-purple-500/20 text-purple-300 border border-purple-500/30 font-bold px-3 py-1 rounded-full text-sm">
            {result.finalPrestige} prestige points
          </span>
        </p>
        <button
          onClick={handleReturn}
          className="px-8 py-3 bg-gradient-to-r from-blue-600 to-blue-700 text-white rounded-xl
            hover:from-blue-700 hover:to-blue-800 font-semibold shadow-md
            transition-all duration-200 hover:shadow-lg"
        >
          Return to Lobby
        </button>
      </div>
    </div>
  );
}
