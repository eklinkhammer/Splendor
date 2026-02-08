import type { ReactNode } from 'react';
import type { PublicPlayer, PlayerId, CardId } from '../../types';
import { useGameStore } from '../../stores/gameStore';
import { PlayerCard } from './PlayerCard';

interface Props {
  players: PublicPlayer[];
  selfPlayerId: PlayerId | null;
  currentPlayerIndex: number;
  onReservedCardClick?: (cardId: string) => void;
  selectedCardId?: CardId | null;
  selectedCardOverlay?: ReactNode;
}

export function PlayerArea({ players, selfPlayerId, currentPlayerIndex, onReservedCardClick, selectedCardId, selectedCardOverlay }: Props) {
  const lastMove = useGameStore((s) => s.lastMove);

  return (
    <div className="space-y-2">
      {players.map((player, idx) => (
        <PlayerCard
          key={player.ppPlayerId}
          player={player}
          isSelf={player.ppPlayerId === selfPlayerId}
          isActive={idx === currentPlayerIndex}
          onReservedCardClick={
            player.ppPlayerId === selfPlayerId ? onReservedCardClick : undefined
          }
          selectedCardId={selectedCardId}
          selectedCardOverlay={player.ppPlayerId === selfPlayerId ? selectedCardOverlay : undefined}
          lastMoveText={
            player.ppPlayerId === lastMove?.playerId && idx !== currentPlayerIndex
              ? lastMove.description
              : undefined
          }
        />
      ))}
    </div>
  );
}
