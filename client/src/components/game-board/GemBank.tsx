import type { GemCollection, TokenType } from '../../types';
import { ALL_TOKEN_TYPES } from '../../types';
import { GemToken } from './GemToken';

interface Props {
  bank: GemCollection;
  onGemClick?: (token: TokenType) => void;
  selectedGems?: TokenType[];
}

export function GemBank({ bank, onGemClick, selectedGems = [] }: Props) {
  return (
    <div className="flex gap-2 justify-center">
      {ALL_TOKEN_TYPES.map((token) => {
        const count = bank[token] ?? 0;
        return (
          <GemToken
            key={token}
            token={token}
            count={count}
            onClick={onGemClick && count > 0 ? () => onGemClick(token) : undefined}
            selected={selectedGems.includes(token)}
          />
        );
      })}
    </div>
  );
}
