import type { Card, GemColor } from '../../types';
import { ALL_GEM_COLORS } from '../../types';

const BONUS_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-white border-gray-300',
  Sapphire: 'bg-blue-600',
  Emerald: 'bg-green-600',
  Ruby: 'bg-red-600',
  Onyx: 'bg-gray-800',
};

const COST_DOT_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-white border border-gray-400 text-gray-800',
  Sapphire: 'bg-blue-600 text-white',
  Emerald: 'bg-green-600 text-white',
  Ruby: 'bg-red-600 text-white',
  Onyx: 'bg-gray-800 text-white',
};

interface Props {
  card: Card;
  onClick?: () => void;
  highlight?: boolean;
}

export function CardDisplay({ card, onClick, highlight }: Props) {
  const costEntries = ALL_GEM_COLORS.filter((c) => (card.cardCost[c] ?? 0) > 0);

  return (
    <button
      type="button"
      onClick={onClick}
      disabled={!onClick}
      className={`w-24 h-32 border-2 rounded-lg p-1.5 flex flex-col justify-between
        ${highlight ? 'border-blue-400 shadow-lg shadow-blue-200' : 'border-gray-300'}
        ${onClick ? 'cursor-pointer hover:border-blue-300 hover:shadow-md transition-all' : 'cursor-default'}
        bg-gray-50`}
    >
      {/* Top: prestige + bonus */}
      <div className="flex items-center justify-between">
        <span className="text-sm font-bold text-gray-700">
          {card.cardPrestige > 0 ? card.cardPrestige : ''}
        </span>
        <div className={`w-5 h-5 rounded-full border ${BONUS_COLORS[card.cardBonus]}`} />
      </div>

      {/* Bottom: cost */}
      <div className="flex flex-col gap-0.5 items-start">
        {costEntries.map((color) => (
          <div key={color} className="flex items-center gap-1">
            <div className={`w-4 h-4 rounded-full text-[10px] flex items-center justify-center font-bold ${COST_DOT_COLORS[color]}`}>
              {card.cardCost[color]}
            </div>
          </div>
        ))}
      </div>
    </button>
  );
}
