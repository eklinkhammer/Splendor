import type { PublicBoard, CardId, Tier, NobleId } from '../../types';
import { NobleRow } from './NobleRow';
import { TierRow } from './TierRow';
import { GemBank } from './GemBank';
import type { TokenType } from '../../types';

interface Props {
  board: PublicBoard;
  onCardClick?: (cardId: CardId) => void;
  onDeckClick?: (tier: Tier) => void;
  onGemClick?: (token: TokenType) => void;
  onNobleClick?: (nobleId: NobleId) => void;
  selectedGems?: TokenType[];
  highlightCards?: CardId[];
  highlightNobles?: NobleId[];
}

export function GameBoard({
  board,
  onCardClick,
  onDeckClick,
  onGemClick,
  onNobleClick,
  selectedGems,
  highlightCards,
  highlightNobles,
}: Props) {
  return (
    <div className="space-y-3">
      <NobleRow
        nobles={board.publicNobles}
        onNobleClick={onNobleClick}
        highlightNobles={highlightNobles}
      />
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
      <GemBank bank={board.publicBank} onGemClick={onGemClick} selectedGems={selectedGems} />
    </div>
  );
}
