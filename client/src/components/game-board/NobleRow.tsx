import type { Noble, NobleId, GemColor } from '../../types';
import { ALL_GEM_COLORS } from '../../types';

const REQ_COLORS: Record<GemColor, string> = {
  Diamond: 'bg-white border border-gray-400 text-gray-800',
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
    <div className="flex gap-2 justify-center">
      {nobles.map((noble) => {
        const reqs = ALL_GEM_COLORS.filter((c) => (noble.nobleRequirement[c] ?? 0) > 0);
        return (
          <button
            key={noble.nobleId}
            type="button"
            onClick={onNobleClick ? () => onNobleClick(noble.nobleId) : undefined}
            disabled={!onNobleClick}
            className={`w-20 h-24 border-2 rounded-lg p-1.5 flex flex-col justify-between bg-purple-50
              ${highlightNobles.includes(noble.nobleId) ? 'border-purple-400 shadow-lg' : 'border-purple-200'}
              ${onNobleClick ? 'cursor-pointer hover:border-purple-400' : 'cursor-default'}`}
          >
            <span className="text-sm font-bold text-purple-800 self-start">{noble.noblePrestige}</span>
            <div className="flex flex-wrap gap-0.5">
              {reqs.map((color) => (
                <div
                  key={color}
                  className={`w-5 h-5 rounded-full text-[10px] flex items-center justify-center font-bold ${REQ_COLORS[color]}`}
                >
                  {noble.nobleRequirement[color]}
                </div>
              ))}
            </div>
          </button>
        );
      })}
    </div>
  );
}
