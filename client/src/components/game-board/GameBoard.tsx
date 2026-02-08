import type { ReactNode } from 'react';
import type { PublicBoard, CardId, Tier, GemColor } from '../../types';
import { NobleRow } from './NobleRow';
import { TierRow } from './TierRow';
import { GemBank } from './GemBank';

interface Props {
  board: PublicBoard;
  onCardClick?: (cardId: CardId) => void;
  onDeckClick?: (tier: Tier) => void;
  highlightCards?: CardId[];
  selectedCardId?: CardId | null;
  selectedCardOverlay?: ReactNode;
  selectedDeckTier?: Tier | null;
  selectedDeckOverlay?: ReactNode;
  reservableDeckTiers?: Set<Tier>;
  onBankGemClick?: (color: GemColor) => void;
  selectedGemCounts?: Partial<Record<GemColor, number>>;
  availableGemColors?: Set<GemColor>;
  bankTakeAction?: ReactNode;
}

export function GameBoard({
  board,
  onCardClick,
  onDeckClick,
  highlightCards,
  selectedCardId,
  selectedCardOverlay,
  selectedDeckTier,
  selectedDeckOverlay,
  reservableDeckTiers,
  onBankGemClick,
  selectedGemCounts,
  availableGemColors,
  bankTakeAction,
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
          selectedCardOverlay={selectedCardOverlay}
          isDeckSelected={selectedDeckTier === 'Tier3'}
          selectedDeckOverlay={selectedDeckOverlay}
          isDeckReservable={reservableDeckTiers?.has('Tier3')}
        />
        <TierRow
          tier="Tier2"
          row={board.publicTier2}
          onCardClick={onCardClick}
          onDeckClick={onDeckClick ? () => onDeckClick('Tier2') : undefined}
          highlightCards={highlightCards}
          selectedCardId={selectedCardId}
          selectedCardOverlay={selectedCardOverlay}
          isDeckSelected={selectedDeckTier === 'Tier2'}
          selectedDeckOverlay={selectedDeckOverlay}
          isDeckReservable={reservableDeckTiers?.has('Tier2')}
        />
        <TierRow
          tier="Tier1"
          row={board.publicTier1}
          onCardClick={onCardClick}
          onDeckClick={onDeckClick ? () => onDeckClick('Tier1') : undefined}
          highlightCards={highlightCards}
          selectedCardId={selectedCardId}
          selectedCardOverlay={selectedCardOverlay}
          isDeckSelected={selectedDeckTier === 'Tier1'}
          selectedDeckOverlay={selectedDeckOverlay}
          isDeckReservable={reservableDeckTiers?.has('Tier1')}
        />
      </div>

      {/* Divider */}
      <div className="border-t border-white/10" />

      {/* Gem bank */}
      <GemBank
        bank={board.publicBank}
        onGemClick={onBankGemClick}
        selectedGemCounts={selectedGemCounts}
        availableGemColors={availableGemColors}
        takeAction={bankTakeAction}
      />
    </div>
  );
}
