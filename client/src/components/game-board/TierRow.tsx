import type { PublicTierRow, Tier, CardId } from '../../types';
import { CardDisplay } from './CardDisplay';

const TIER_COLORS: Record<Tier, string> = {
  Tier1: 'bg-green-100 text-green-800',
  Tier2: 'bg-yellow-100 text-yellow-800',
  Tier3: 'bg-blue-100 text-blue-800',
};

interface Props {
  tier: Tier;
  row: PublicTierRow;
  onCardClick?: (cardId: CardId) => void;
  onDeckClick?: () => void;
  highlightCards?: CardId[];
}

export function TierRow({ tier, row, onCardClick, onDeckClick, highlightCards = [] }: Props) {
  return (
    <div className="flex items-center gap-2">
      {/* Deck */}
      <button
        type="button"
        onClick={onDeckClick}
        disabled={!onDeckClick || row.publicDeckCount === 0}
        className={`w-24 h-32 rounded-lg border-2 border-dashed flex flex-col items-center justify-center
          ${TIER_COLORS[tier]}
          ${onDeckClick && row.publicDeckCount > 0 ? 'cursor-pointer hover:border-blue-400' : 'cursor-default opacity-60'}`}
      >
        <span className="text-2xl font-bold">{row.publicDeckCount}</span>
        <span className="text-xs">{tier.replace('Tier', 'Tier ')}</span>
      </button>

      {/* Display cards */}
      {row.publicDisplay.map((card) => (
        <CardDisplay
          key={card.cardId}
          card={card}
          onClick={onCardClick ? () => onCardClick(card.cardId) : undefined}
          highlight={highlightCards.includes(card.cardId)}
        />
      ))}

      {/* Empty slots */}
      {Array.from({ length: Math.max(0, 4 - row.publicDisplay.length) }).map((_, i) => (
        <div key={`empty-${i}`} className="w-24 h-32 rounded-lg border-2 border-dashed border-gray-200" />
      ))}
    </div>
  );
}
