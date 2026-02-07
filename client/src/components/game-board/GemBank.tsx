import type { GemCollection, TokenType } from '../../types';
import { ALL_TOKEN_TYPES } from '../../types';
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
  onGemClick?: (token: TokenType) => void;
  selectedGems?: TokenType[];
}

export function GemBank({ bank, onGemClick, selectedGems = [] }: Props) {
  return (
    <div className="bg-[var(--board-felt-light)] rounded-xl px-4 py-3">
      <div className="text-xs font-semibold text-gray-400 uppercase tracking-wider mb-2 text-center">
        Gem Bank
      </div>
      <div className="flex gap-3 justify-center">
        {ALL_TOKEN_TYPES.map((token) => {
          const count = bank[token] ?? 0;
          return (
            <div key={token} className="flex flex-col items-center gap-1">
              <GemToken
                token={token}
                count={count}
                onClick={onGemClick && count > 0 ? () => onGemClick(token) : undefined}
                selected={selectedGems.includes(token)}
              />
              <span className="text-[10px] text-gray-400 font-medium">{TOKEN_LABELS[token]}</span>
            </div>
          );
        })}
      </div>
    </div>
  );
}
