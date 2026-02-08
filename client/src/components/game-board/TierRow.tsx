import type { ReactNode } from 'react';
import type { PublicTierRow, Tier, CardId } from '../../types';
import { CardDisplay } from './CardDisplay';

const TIER_DECK_STYLES: Record<Tier, string> = {
  Tier1: 'from-green-600 to-green-800 border-green-500',
  Tier2: 'from-amber-500 to-amber-700 border-amber-400',
  Tier3: 'from-indigo-500 to-indigo-800 border-indigo-400',
};

const TIER_BADGE_STYLES: Record<Tier, string> = {
  Tier1: 'bg-green-400 text-green-900',
  Tier2: 'bg-amber-300 text-amber-900',
  Tier3: 'bg-indigo-300 text-indigo-900',
};

const TIER_LABELS: Record<Tier, string> = {
  Tier1: 'I',
  Tier2: 'II',
  Tier3: 'III',
};

interface Props {
  tier: Tier;
  row: PublicTierRow;
  onCardClick?: (cardId: CardId) => void;
  onDeckClick?: () => void;
  highlightCards?: CardId[];
  selectedCardId?: CardId | null;
  selectedCardOverlay?: ReactNode;
  isDeckSelected?: boolean;
  selectedDeckOverlay?: ReactNode;
  isDeckReservable?: boolean;
}

export function TierRow({ tier, row, onCardClick, onDeckClick, highlightCards = [], selectedCardId, selectedCardOverlay, isDeckSelected, selectedDeckOverlay, isDeckReservable }: Props) {
  return (
    <div className="flex items-center gap-1 sm:gap-1.5 lg:gap-2">
      {/* Tier label */}
      <div className={`w-6 h-6 sm:w-7 sm:h-7 lg:w-8 lg:h-8 rounded-md flex items-center justify-center text-xs sm:text-sm font-extrabold ${TIER_BADGE_STYLES[tier]}`}>
        {TIER_LABELS[tier]}
      </div>

      {/* Deck */}
      <div className={`relative ${isDeckSelected ? 'overflow-visible' : ''}`}>
        <button
          type="button"
          onClick={onDeckClick}
          disabled={!onDeckClick || row.publicDeckCount === 0}
          className={`w-[var(--card-width)] h-[var(--card-height)] rounded-lg border-2 flex flex-col items-center justify-center
            bg-gradient-to-br ${TIER_DECK_STYLES[tier]} shadow-md
            ${isDeckSelected ? 'ring-2 ring-amber-400 ring-offset-2 ring-offset-gray-900 scale-105 border-amber-400 shadow-lg shadow-amber-300/50' : isDeckReservable ? 'border-amber-400 shadow-lg shadow-amber-300/50' : ''}
            ${onDeckClick && row.publicDeckCount > 0 ? 'cursor-pointer hover:shadow-lg hover:brightness-110 transition-all' : 'cursor-default opacity-60'}`}
        >
          <span className="text-lg sm:text-2xl lg:text-3xl font-bold text-white/90 drop-shadow-sm">{row.publicDeckCount}</span>
          <span className={`text-[10px] font-bold px-2 py-0.5 rounded-full mt-1 ${TIER_BADGE_STYLES[tier]}`}>
            {tier.replace('Tier', 'Tier ')}
          </span>
        </button>
        {isDeckSelected && selectedDeckOverlay}
      </div>

      {/* Display cards */}
      {row.publicDisplay.map((card) => {
        const isSelected = card.cardId === selectedCardId;
        return (
          <div key={card.cardId} className={`relative ${isSelected ? 'overflow-visible' : ''}`}>
            <CardDisplay
              card={card}
              onClick={onCardClick ? () => onCardClick(card.cardId) : undefined}
              highlight={highlightCards.includes(card.cardId)}
              selected={isSelected}
            />
            {isSelected && selectedCardOverlay}
          </div>
        );
      })}

      {/* Empty slots */}
      {Array.from({ length: Math.max(0, 4 - row.publicDisplay.length) }).map((_, i) => (
        <div key={`empty-${i}`} className="w-[var(--card-width)] h-[var(--card-height)] rounded-lg border-2 border-dashed border-white/20 bg-white/5" />
      ))}
    </div>
  );
}
