import type { PublicGameView } from '../../types';

interface Props {
  gameView: PublicGameView;
  selfPlayerId: string | null;
}

export function GameStatus({ gameView, selfPlayerId }: Props) {
  const currentPlayer = gameView.pgvPlayers[gameView.pgvCurrentPlayer];
  const isMyTurn = currentPlayer?.ppPlayerId === selfPlayerId;

  const phaseLabel =
    gameView.pgvPhase.tag === 'FinalRound'
      ? 'Final Round!'
      : gameView.pgvPhase.tag === 'Finished'
        ? 'Game Over'
        : '';

  return (
    <div className="flex items-center justify-between px-4 py-2 bg-gray-800 text-white rounded-lg">
      <span className="text-sm">Turn {gameView.pgvTurnNumber}</span>
      <span className={`font-semibold ${isMyTurn ? 'text-green-400' : ''}`}>
        {isMyTurn ? 'Your Turn' : `${currentPlayer?.ppPlayerName ?? '?'}'s Turn`}
      </span>
      {phaseLabel && (
        <span className="text-sm text-yellow-400 font-medium">{phaseLabel}</span>
      )}
    </div>
  );
}
