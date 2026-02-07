import type { Noble, NobleId, GemColor } from '../../types';
import { ALL_GEM_COLORS } from '../../types';

const REQ_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-white border border-gray-300 text-gray-800',
  Sapphire: 'bg-blue-600 text-white',
  Emerald: 'bg-green-600 text-white',
  Ruby: 'bg-red-600 text-white',
  Onyx: 'bg-gray-800 text-white',
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
      <div className="flex gap-3 justify-center">
        {nobles.map((noble) => {
          const reqs = ALL_GEM_COLORS.filter((c) => (noble.nobleRequirement[c] ?? 0) > 0);
          const isHighlighted = highlightNobles.includes(noble.nobleId);
          return (
            <button
              key={noble.nobleId}
              type="button"
              onClick={onNobleClick ? () => onNobleClick(noble.nobleId) : undefined}
              disabled={!onNobleClick}
              className={`w-24 h-28 rounded-lg p-2 flex flex-col justify-between
                bg-gradient-to-b from-amber-50 to-amber-100
                border-2 transition-all duration-200 shadow-md
                ${isHighlighted ? 'border-amber-400 shadow-lg shadow-amber-300/50' : 'border-amber-300/70'}
                ${onNobleClick ? 'cursor-pointer hover:border-amber-400 hover:-translate-y-0.5 hover:shadow-lg' : 'cursor-default'}`}
            >
              <div className="flex items-center justify-between">
                <span className="text-lg font-extrabold text-purple-800">{noble.noblePrestige}</span>
                <span className="text-sm">👑</span>
              </div>
              <div className="flex gap-1 justify-center">
                {reqs.map((color) => (
                  <div
                    key={color}
                    className={`w-6 h-6 rounded-full text-xs flex items-center justify-center font-bold shadow-sm ${REQ_COLORS[color]}`}
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
