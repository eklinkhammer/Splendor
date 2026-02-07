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

  const phaseBadge =
    gameView.pgvPhase.tag === 'FinalRound'
      ? 'bg-amber-500 text-amber-950'
      : gameView.pgvPhase.tag === 'Finished'
        ? 'bg-red-500 text-white'
        : '';

  return (
    <div className="flex items-center justify-between px-5 py-3 bg-gradient-to-r from-gray-800 to-gray-900 text-white rounded-xl shadow-lg">
      <span className="text-sm bg-gray-700 px-3 py-1 rounded-full font-medium min-w-[5rem] text-center">
        Turn {gameView.pgvTurnNumber}
      </span>
      <div className="flex items-center gap-2">
        {isMyTurn && (
          <span className="relative flex h-2.5 w-2.5">
            <span className="animate-ping absolute inline-flex h-full w-full rounded-full bg-green-400 opacity-75" />
            <span className="relative inline-flex rounded-full h-2.5 w-2.5 bg-green-500" />
          </span>
        )}
        <span className={`text-lg font-bold ${isMyTurn ? 'text-green-400' : ''}`}>
          {isMyTurn ? 'Your Turn' : `${currentPlayer?.ppPlayerName ?? '?'}'s Turn`}
        </span>
      </div>
      <span className={`text-sm font-bold px-3 py-1 rounded-full min-w-[5rem] text-center ${phaseBadge}`}>
        {phaseLabel}
      </span>
    </div>
  );
}
