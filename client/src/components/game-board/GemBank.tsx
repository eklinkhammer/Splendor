import type { ReactNode } from 'react';
import type { GemCollection, GemColor, TokenType } from '../../types';
import { ALL_GEM_COLORS } from '../../types';
import { GemToken } from './GemToken';

const TOKEN_LABELS: Record<TokenType, string> = {
  Diamond: 'Dia',
  Sapphire: 'Sap',
  Emerald: 'Eme',
  Ruby: 'Rub',
  Onyx: 'Onx',
  Gold: 'Gold',
};

interface Props {
  bank: GemCollection;
  onGemClick?: (color: GemColor) => void;
  selectedGemCounts?: Partial<Record<GemColor, number>>;
  availableGemColors?: Set<GemColor>;
  takeAction?: ReactNode;
}

export function GemBank({ bank, onGemClick, selectedGemCounts = {}, availableGemColors, takeAction }: Props) {
  const goldCount = bank['Gold'] ?? 0;
  return (
    <div className="bg-[var(--board-felt-light)] rounded-xl px-4 py-3">
      <div className="text-xs font-semibold text-gray-400 uppercase tracking-wider mb-2 text-center">
        Gem Bank
      </div>
      <div className="flex gap-3 justify-center">
        {ALL_GEM_COLORS.map((color) => {
          const count = bank[color] ?? 0;
          const selCount = selectedGemCounts[color] ?? 0;
          const isClickable = onGemClick && (selCount > 0 || (count > 0 && (!availableGemColors || availableGemColors.has(color))));
          return (
            <div key={color} className="flex flex-col items-center gap-1">
              <GemToken
                token={color}
                count={count}
                onClick={isClickable ? () => onGemClick(color) : undefined}
                selectedCount={selCount}
              />
              <span className="text-[10px] text-gray-400 font-medium">{TOKEN_LABELS[color]}</span>
            </div>
          );
        })}
        <div className="flex flex-col items-center gap-1">
          <GemToken token="Gold" count={goldCount} />
          <span className="text-[10px] text-gray-400 font-medium">{TOKEN_LABELS['Gold']}</span>
        </div>
      </div>
      {takeAction}
    </div>
  );
}
