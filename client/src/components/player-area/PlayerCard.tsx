import type { PublicPlayer, GemColor, CardId } from '../../types';
import { ALL_GEM_COLORS, ALL_TOKEN_TYPES, countBonuses } from '../../types';
import { GemToken } from '../game-board/GemToken';
import { CardDisplay } from '../game-board/CardDisplay';

const BONUS_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-[var(--gem-diamond)] border border-gray-500 text-gray-900',
  Sapphire: 'bg-[var(--gem-sapphire)] text-white',
  Emerald: 'bg-[var(--gem-emerald)] text-white',
  Ruby: 'bg-[var(--gem-ruby)] text-white',
  Onyx: 'bg-[var(--gem-onyx)] text-white',
};

const BONUS_LABELS: Record<GemColor, string> = {
  Diamond: 'D',
  Sapphire: 'S',
  Emerald: 'E',
  Ruby: 'R',
  Onyx: 'O',
};

interface Props {
  player: PublicPlayer;
  isSelf: boolean;
  isActive: boolean;
  onReservedCardClick?: (cardId: string) => void;
  selectedCardId?: CardId | null;
  lastMoveText?: string;
}

export function PlayerCard({ player, isSelf, isActive, onReservedCardClick, selectedCardId, lastMoveText }: Props) {
  const bonuses = countBonuses(player.ppPurchased);
  const hasTokens = ALL_TOKEN_TYPES.some((t) => (player.ppTokens[t] ?? 0) > 0);
  const hasBonuses = ALL_GEM_COLORS.some((c) => (bonuses[c] ?? 0) > 0);

  return (
    <div className={`rounded-xl overflow-hidden shadow-md transition-all duration-200
      ${isActive ? 'ring-2 ring-green-400 shadow-green-500/20' : 'ring-1 ring-gray-600'}`}
    >
      {/* Header bar */}
      <div className={`px-4 py-2.5 flex items-center justify-between
        ${isActive ? 'bg-gradient-to-r from-green-600 to-green-700 text-white' : 'bg-gradient-to-r from-gray-700 to-gray-800 text-gray-100'}`}
      >
        <div className="flex items-center gap-2">
          <div className="flex flex-col">
            <div className="flex items-center gap-2">
              <span className="font-bold">{player.ppPlayerName}</span>
              {isSelf && <span className="text-xs bg-white/20 px-1.5 py-0.5 rounded text-white/90">you</span>}
            </div>
            {lastMoveText && (
              <div className="text-[10px] text-gray-400 truncate max-w-[160px]">
                {lastMoveText}
              </div>
            )}
          </div>
        </div>
        <span className="bg-purple-500 text-white text-sm font-bold px-2.5 py-0.5 rounded-full shadow-sm">
          {player.ppPrestige} VP
        </span>
      </div>

      {/* Body */}
      <div className={`p-3 space-y-2.5 ${isActive ? 'bg-green-900/30' : 'bg-gray-800'}`}>
        {/* Tokens */}
        {hasTokens && (
          <div>
            <div className="text-[10px] font-semibold text-gray-400 uppercase tracking-wider mb-1">Tokens</div>
            <div className="flex gap-1.5">
              {ALL_TOKEN_TYPES.map((token) => {
                const count = player.ppTokens[token] ?? 0;
                if (count === 0) return null;
                return <GemToken key={token} token={token} count={count} size="sm" />;
              })}
            </div>
          </div>
        )}

        {/* Bonuses */}
        {hasBonuses && (
          <div>
            <div className="text-[10px] font-semibold text-gray-400 uppercase tracking-wider mb-1">Bonuses</div>
            <div className="flex gap-1">
              {ALL_GEM_COLORS.map((color) => {
                const count = bonuses[color] ?? 0;
                if (count === 0) return null;
                return (
                  <div
                    key={color}
                    className={`w-7 h-7 rounded-md text-xs flex items-center justify-center font-bold shadow-sm ${BONUS_COLORS[color]}`}
                    title={`${color}: ${count}`}
                  >
                    <span className="text-[9px] opacity-60 mr-px">{BONUS_LABELS[color]}</span>{count}
                  </div>
                );
              })}
            </div>
          </div>
        )}

        {/* Nobles */}
        {player.ppNobles.length > 0 && (
          <div>
            <div className="text-[10px] font-semibold text-gray-400 uppercase tracking-wider mb-1">Nobles</div>
            <div className="flex gap-1">
              {player.ppNobles.map((noble) => (
                <div
                  key={noble.nobleId}
                  className="w-7 h-7 rounded-md bg-gradient-to-b from-amber-600 to-amber-700 border border-amber-500/50
                    flex items-center justify-center text-xs font-bold text-white shadow-sm"
                  title={`Noble: ${noble.noblePrestige} VP`}
                >
                  👑
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Reserved cards (self only) */}
        {isSelf && player.ppReserved && player.ppReserved.length > 0 && (
          <div>
            <div className="flex items-center gap-2 mb-1">
              <span className="text-[10px] font-semibold text-gray-400 uppercase tracking-wider">Reserved</span>
              <span className="text-[10px] bg-gray-600 text-gray-300 font-bold px-1.5 rounded-full">
                {player.ppReserved.length}
              </span>
            </div>
            <div className="flex gap-1.5">
              {player.ppReserved.map((card) => (
                <CardDisplay
                  key={card.cardId}
                  card={card}
                  onClick={onReservedCardClick ? () => onReservedCardClick(card.cardId) : undefined}
                  selected={card.cardId === selectedCardId}
                />
              ))}
            </div>
          </div>
        )}

        {/* Reserved count (opponent) */}
        {!isSelf && player.ppReservedCount > 0 && (
          <div className="text-xs text-gray-400 flex items-center gap-1">
            Reserved
            <span className="bg-gray-600 text-gray-300 font-bold px-1.5 rounded-full text-[10px]">
              {player.ppReservedCount}
            </span>
          </div>
        )}
      </div>
    </div>
  );
}
