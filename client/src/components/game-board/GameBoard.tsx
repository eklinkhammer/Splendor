import type { PublicBoard, CardId, Tier } from '../../types';
import { NobleRow } from './NobleRow';
import { TierRow } from './TierRow';
import { GemBank } from './GemBank';

interface Props {
  board: PublicBoard;
  onCardClick?: (cardId: CardId) => void;
  onDeckClick?: (tier: Tier) => void;
  highlightCards?: CardId[];
}

export function GameBoard({
  board,
  onCardClick,
  onDeckClick,
  highlightCards,
}: Props) {
  return (
    <div className="space-y-3">
      <NobleRow nobles={board.publicNobles} />
      <TierRow
        tier="Tier3"
        row={board.publicTier3}
        onCardClick={onCardClick}
        onDeckClick={onDeckClick ? () => onDeckClick('Tier3') : undefined}
        highlightCards={highlightCards}
      />
      <TierRow
        tier="Tier2"
        row={board.publicTier2}
        onCardClick={onCardClick}
        onDeckClick={onDeckClick ? () => onDeckClick('Tier2') : undefined}
        highlightCards={highlightCards}
      />
      <TierRow
        tier="Tier1"
        row={board.publicTier1}
        onCardClick={onCardClick}
        onDeckClick={onDeckClick ? () => onDeckClick('Tier1') : undefined}
        highlightCards={highlightCards}
      />
      <GemBank bank={board.publicBank} />
    </div>
  );
}
