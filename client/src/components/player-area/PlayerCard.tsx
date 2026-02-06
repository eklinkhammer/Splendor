import type { PublicPlayer, GemColor } from '../../types';
import { ALL_GEM_COLORS, ALL_TOKEN_TYPES, countBonuses } from '../../types';
import { GemToken } from '../game-board/GemToken';
import { CardDisplay } from '../game-board/CardDisplay';

const BONUS_DOT_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-white border border-gray-400 text-gray-800',
  Sapphire: 'bg-blue-600 text-white',
  Emerald: 'bg-green-600 text-white',
  Ruby: 'bg-red-600 text-white',
  Onyx: 'bg-gray-800 text-white',
};

interface Props {
  player: PublicPlayer;
  isSelf: boolean;
  isActive: boolean;
  onReservedCardClick?: (cardId: string) => void;
}

export function PlayerCard({ player, isSelf, isActive, onReservedCardClick }: Props) {
  const bonuses = countBonuses(player.ppPurchased);

  return (
    <div className={`border-2 rounded-lg p-3 ${isActive ? 'border-green-400 bg-green-50' : 'border-gray-200 bg-white'}`}>
      {/* Header */}
      <div className="flex items-center justify-between mb-2">
        <div className="flex items-center gap-2">
          <span className="font-semibold">{player.ppPlayerName}</span>
          {isSelf && <span className="text-xs text-blue-500">(you)</span>}
        </div>
        <span className="text-lg font-bold text-purple-700">{player.ppPrestige} VP</span>
      </div>

      {/* Tokens */}
      <div className="flex gap-1 mb-2">
        {ALL_TOKEN_TYPES.map((token) => {
          const count = player.ppTokens[token] ?? 0;
          if (count === 0) return null;
          return <GemToken key={token} token={token} count={count} size="sm" />;
        })}
      </div>

      {/* Bonuses */}
      <div className="flex gap-1 mb-2">
        {ALL_GEM_COLORS.map((color) => {
          const count = bonuses[color] ?? 0;
          if (count === 0) return null;
          return (
            <div
              key={color}
              className={`w-6 h-6 rounded text-xs flex items-center justify-center font-bold ${BONUS_DOT_COLORS[color]}`}
            >
              {count}
            </div>
          );
        })}
      </div>

      {/* Nobles */}
      {player.ppNobles.length > 0 && (
        <div className="text-xs text-purple-600 mb-1">
          Nobles: {player.ppNobles.length}
        </div>
      )}

      {/* Reserved cards (self only) */}
      {isSelf && player.ppReserved && player.ppReserved.length > 0 && (
        <div>
          <div className="text-xs text-gray-500 mb-1">Reserved ({player.ppReserved.length})</div>
          <div className="flex gap-1">
            {player.ppReserved.map((card) => (
              <CardDisplay
                key={card.cardId}
                card={card}
                onClick={onReservedCardClick ? () => onReservedCardClick(card.cardId) : undefined}
              />
            ))}
          </div>
        </div>
      )}

      {/* Reserved count (opponent) */}
      {!isSelf && player.ppReservedCount > 0 && (
        <div className="text-xs text-gray-500">Reserved: {player.ppReservedCount}</div>
      )}
    </div>
  );
}
