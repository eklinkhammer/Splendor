import type { Noble, NobleId, GemColor } from '../../types';
import { ALL_GEM_COLORS } from '../../types';

const REQ_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-[var(--gem-diamond)] border border-gray-500 text-gray-900',
  Sapphire: 'bg-[var(--gem-sapphire)] text-white',
  Emerald: 'bg-[var(--gem-emerald)] text-white',
  Ruby: 'bg-[var(--gem-ruby)] text-white',
  Onyx: 'bg-[var(--gem-onyx)] text-white',
};

interface Props {
  nobles: Noble[];
  onNobleClick?: (nobleId: NobleId) => void;
  highlightNobles?: NobleId[];
}

export function NobleRow({ nobles, onNobleClick, highlightNobles = [] }: Props) {
  return (
    <div>
      <div className="text-xs font-semibold text-amber-200/80 uppercase tracking-wider mb-1.5 text-center">
        Nobles
      </div>
      <div className="flex gap-1.5 sm:gap-2 lg:gap-3 justify-center">
        {nobles.map((noble) => {
          const reqs = ALL_GEM_COLORS.filter((c) => (noble.nobleRequirement[c] ?? 0) > 0);
          const isHighlighted = highlightNobles.includes(noble.nobleId);
          return (
            <button
              key={noble.nobleId}
              type="button"
              onClick={onNobleClick ? () => onNobleClick(noble.nobleId) : undefined}
              disabled={!onNobleClick}
              className={`w-[var(--noble-width)] h-[var(--noble-height)] rounded-lg p-1.5 sm:p-2 flex flex-col justify-between
                bg-gradient-to-b from-amber-700 to-amber-800
                border-2 transition-all duration-200 shadow-md
                ${isHighlighted ? 'border-amber-400 shadow-lg shadow-amber-400/30' : 'border-amber-600/70'}
                ${onNobleClick ? 'cursor-pointer hover:border-amber-400 hover:-translate-y-0.5 hover:shadow-lg' : 'cursor-default'}`}
            >
              <div className="flex items-center justify-between">
                <span className="text-sm sm:text-base lg:text-lg font-extrabold text-purple-200">{noble.noblePrestige}</span>
                <span className="text-xs sm:text-sm">👑</span>
              </div>
              <div className="flex gap-0.5 sm:gap-1 justify-center">
                {reqs.map((color) => (
                  <div
                    key={color}
                    className={`w-4.5 h-4.5 sm:w-5 sm:h-5 lg:w-6 lg:h-6 rounded-full text-[10px] sm:text-xs flex items-center justify-center font-bold shadow-sm ${REQ_COLORS[color]}`}
                  >
                    {noble.nobleRequirement[color]}
                  </div>
                ))}
              </div>
            </button>
          );
        })}
      </div>
    </div>
  );
}
