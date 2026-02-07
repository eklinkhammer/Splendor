import type { Card, GemColor } from '../../types';
import { ALL_GEM_COLORS } from '../../types';

const HEADER_GRADIENTS: Record<GemColor, string> = {
  Diamond: 'from-gray-100 to-gray-300',
  Sapphire: 'from-blue-400 to-blue-700',
  Emerald: 'from-green-400 to-green-700',
  Ruby: 'from-red-400 to-red-700',
  Onyx: 'from-gray-600 to-gray-900',
};

const BONUS_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-white border border-gray-300 shadow-inner',
  Sapphire: 'bg-blue-500 border border-blue-300',
  Emerald: 'bg-green-500 border border-green-300',
  Ruby: 'bg-red-500 border border-red-300',
  Onyx: 'bg-gray-700 border border-gray-500',
};

const COST_PILL_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-[var(--gem-diamond)] border border-gray-300 text-gray-800',
  Sapphire: 'bg-[var(--gem-sapphire)] text-white',
  Emerald: 'bg-[var(--gem-emerald)] text-white',
  Ruby: 'bg-[var(--gem-ruby)] text-white',
  Onyx: 'bg-[var(--gem-onyx)] text-white',
};

const PRESTIGE_TEXT: Record<GemColor, string> = {
  Diamond: 'text-gray-700',
  Sapphire: 'text-white',
  Emerald: 'text-white',
  Ruby: 'text-white',
  Onyx: 'text-white',
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
      className={`w-28 h-[var(--card-height)] rounded-lg overflow-hidden flex flex-col
        border-2 transition-all duration-200
        ${highlight ? 'border-amber-400 shadow-lg shadow-amber-300/50' : 'border-gray-600/80'}
        ${onClick ? 'cursor-pointer hover:-translate-y-1 hover:shadow-lg hover:border-amber-300' : 'cursor-default'}
        bg-[var(--card-bg)] shadow-md`}
    >
      {/* Header band with gradient */}
      <div className={`relative h-[46px] w-full bg-gradient-to-br ${HEADER_GRADIENTS[card.cardBonus]} flex items-start justify-between px-2 pt-1.5`}>
        {/* Prestige number */}
        <span className={`text-xl font-extrabold drop-shadow-sm ${PRESTIGE_TEXT[card.cardBonus]}`}>
          {card.cardPrestige > 0 ? card.cardPrestige : ''}
        </span>
        {/* Bonus gem circle */}
        <div className={`w-7 h-7 rounded-full ${BONUS_COLORS[card.cardBonus]} flex items-center justify-center shadow-sm`}>
          <div className="w-3 h-3 rounded-full bg-white/30" />
        </div>
      </div>

      {/* Card body with cost */}
      <div className="flex-1 p-1.5 flex flex-col justify-end">
        <div className="flex flex-col gap-0.5">
          {costEntries.map((color) => (
            <div key={color} className="flex items-center gap-1">
              <div className={`w-5 h-5 rounded-full text-xs flex items-center justify-center font-bold shadow-sm ${COST_PILL_COLORS[color]}`}>
                {card.cardCost[color]}
              </div>
            </div>
          ))}
        </div>
      </div>
    </button>
  );
}
