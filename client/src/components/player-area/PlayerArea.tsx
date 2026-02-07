import type { PublicPlayer, PlayerId, CardId } from '../../types';
import { PlayerCard } from './PlayerCard';

interface Props {
  players: PublicPlayer[];
  selfPlayerId: PlayerId | null;
  currentPlayerIndex: number;
  onReservedCardClick?: (cardId: string) => void;
  selectedCardId?: CardId | null;
}

export function PlayerArea({ players, selfPlayerId, currentPlayerIndex, onReservedCardClick, selectedCardId }: Props) {
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
        />
      ))}
    </div>
  );
}
