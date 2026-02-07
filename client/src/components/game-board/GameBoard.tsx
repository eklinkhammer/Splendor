import type { PublicBoard, CardId, Tier } from '../../types';
import { NobleRow } from './NobleRow';
import { TierRow } from './TierRow';
import { GemBank } from './GemBank';

interface Props {
  board: PublicBoard;
  onCardClick?: (cardId: CardId) => void;
  onDeckClick?: (tier: Tier) => void;
  highlightCards?: CardId[];
  selectedCardId?: CardId | null;
}

export function GameBoard({
  board,
  onCardClick,
  onDeckClick,
  highlightCards,
  selectedCardId,
}: Props) {
  return (
    <div className="bg-[var(--board-felt)] rounded-xl p-6 shadow-inner space-y-4">
      {/* Nobles section */}
      <NobleRow nobles={board.publicNobles} />

      {/* Divider */}
      <div className="border-t border-white/10" />

      {/* Card tiers */}
      <div className="space-y-3">
        <TierRow
          tier="Tier3"
          row={board.publicTier3}
          onCardClick={onCardClick}
          onDeckClick={onDeckClick ? () => onDeckClick('Tier3') : undefined}
          highlightCards={highlightCards}
          selectedCardId={selectedCardId}
        />
        <TierRow
          tier="Tier2"
          row={board.publicTier2}
          onCardClick={onCardClick}
          onDeckClick={onDeckClick ? () => onDeckClick('Tier2') : undefined}
          highlightCards={highlightCards}
          selectedCardId={selectedCardId}
        />
        <TierRow
          tier="Tier1"
          row={board.publicTier1}
          onCardClick={onCardClick}
          onDeckClick={onDeckClick ? () => onDeckClick('Tier1') : undefined}
          highlightCards={highlightCards}
          selectedCardId={selectedCardId}
        />
      </div>

      {/* Divider */}
      <div className="border-t border-white/10" />

      {/* Gem bank */}
      <GemBank bank={board.publicBank} />
    </div>
  );
}
