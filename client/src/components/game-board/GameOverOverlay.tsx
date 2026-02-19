import { useState } from 'react';
import { useNavigate, useSearchParams } from 'react-router-dom';
import type { GameResult } from '../../types';
import { useSessionStore } from '../../stores/sessionStore';
import { useGameStore } from '../../stores/gameStore';
import { playAgain } from '../../services/api';

interface Props {
  result: GameResult;
}

const RANK_MEDALS = ['🥇', '🥈', '🥉'];

export function GameOverOverlay({ result }: Props) {
  const navigate = useNavigate();
  const [searchParams] = useSearchParams();
  const sessionId = searchParams.get('s');
  const clearSession = useSessionStore((s) => s.clear);
  const setSession = useSessionStore((s) => s.setSession);
  const setGameId = useSessionStore((s) => s.setGameId);
  const playerName = useSessionStore((s) => s.playerName);
  const resetGame = useGameStore((s) => s.reset);
  const gameView = useGameStore((s) => s.gameView);
  const [playingAgain, setPlayingAgain] = useState(false);

  const handleReturn = () => {
    resetGame();
    clearSession();
    navigate('/');
  };

  const handlePlayAgain = async () => {
    if (!gameView || !playerName) return;
    setPlayingAgain(true);
    try {
      // Count AI players (all non-self players are AI in the new model)
      const selfPlayer = gameView.pgvPlayers.find((p) => p.ppReserved !== null);
      const aiCount = gameView.pgvPlayers.filter((p) => p.ppPlayerId !== selfPlayer?.ppPlayerId).length;

      const res = await playAgain(playerName, `${playerName}'s Game`, aiCount);
      resetGame();
      setSession(res.sessionId, playerName);
      setGameId(res.gameId);
      navigate(`/game/${res.gameId}?s=${res.sessionId}`);
    } catch {
      setPlayingAgain(false);
    }
  };

  const rankedPlayers = gameView
    ? [...gameView.pgvPlayers].sort(
        (a, b) => b.ppPrestige - a.ppPrestige || b.ppPurchased.length - a.ppPurchased.length,
      )
    : [];

  return (
    <div className="fixed inset-0 bg-black/60 flex items-center justify-center z-50">
      <div className="bg-gradient-to-b from-gray-800 to-gray-900 rounded-2xl p-4 sm:p-6 lg:p-10 shadow-2xl text-center max-w-[calc(100vw-2rem)] sm:max-w-md border border-amber-500/30">
        <div className="text-5xl mb-3">🏆</div>
        <h2 className="text-2xl font-extrabold text-gray-100 mb-2">Game Over!</h2>
        <p className="text-xl mb-6 text-gray-200">
          <span className="font-bold bg-gradient-to-r from-purple-400 to-purple-600 bg-clip-text text-transparent">
            {result.winnerName}
          </span>{' '}
          wins!
        </p>

        <div className="space-y-2 mb-8 text-left">
          {rankedPlayers.map((player, idx) => {
            const isWinner = idx === 0;
            const medal = RANK_MEDALS[idx] ?? `#${idx + 1}`;
            return (
              <div
                key={player.ppPlayerId}
                className={`flex items-center gap-3 px-4 py-2.5 rounded-xl ${
                  isWinner
                    ? 'bg-amber-500/10 border border-amber-500/30 ring-1 ring-amber-400/30'
                    : 'bg-gray-700/50'
                }`}
              >
                <span className="text-lg w-8 text-center">{medal}</span>
                <span className="font-bold text-gray-200 flex-1 truncate">{player.ppPlayerName}</span>
                <span className="bg-purple-500 text-white text-xs font-bold px-2 py-0.5 rounded-full">
                  {player.ppPrestige} VP
                </span>
                <span className="text-xs text-gray-400">
                  {player.ppPurchased.length} card{player.ppPurchased.length !== 1 ? 's' : ''}
                </span>
                <span className="text-xs text-gray-400">
                  {player.ppNobles.length} noble{player.ppNobles.length !== 1 ? 's' : ''}
                </span>
              </div>
            );
          })}
        </div>

        <div className="flex gap-3 justify-center">
          {sessionId && (
            <button
              onClick={handlePlayAgain}
              disabled={playingAgain}
              className="px-8 py-3 bg-gradient-to-r from-green-600 to-green-700 text-white rounded-xl
                hover:from-green-700 hover:to-green-800 font-semibold shadow-md
                transition-all duration-200 hover:shadow-lg disabled:opacity-50"
            >
              {playingAgain ? 'Setting up...' : 'Play Again'}
            </button>
          )}
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
    </div>
  );
}
