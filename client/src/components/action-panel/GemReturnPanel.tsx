import { useState } from 'react';
import type { GemCollection, ClientMessage, TokenType } from '../../types';
import { ALL_TOKEN_TYPES } from '../../types';
import { GemToken } from '../game-board/GemToken';
import { totalGems, addToCollection, emptyGemCollection } from '../../types';

interface Props {
  amount: number;
  options: GemCollection[];
  playerTokens: GemCollection;
  send: (msg: ClientMessage) => void;
}

export function GemReturnPanel({ amount, options, playerTokens, send }: Props) {
  const [returning, setReturning] = useState<GemCollection>(emptyGemCollection());

  const returnedTotal = totalGems(returning);
  const isComplete = returnedTotal === amount;

  // Check if current selection matches a valid option
  const isValid = isComplete && options.some((opt) =>
    ALL_TOKEN_TYPES.every((t) => (opt[t] ?? 0) === (returning[t] ?? 0)),
  );

  const handleGemClick = (token: TokenType) => {
    const playerHas = playerTokens[token] ?? 0;
    const alreadyReturning = returning[token] ?? 0;
    if (alreadyReturning < playerHas && returnedTotal < amount) {
      setReturning(addToCollection(returning, token, 1));
    }
  };

  const handleUndo = (token: TokenType) => {
    const alreadyReturning = returning[token] ?? 0;
    if (alreadyReturning > 0) {
      setReturning(addToCollection(returning, token, -1));
    }
  };

  const handleSubmit = () => {
    if (isValid) {
      send({ tag: 'ReturnGems', contents: returning });
      setReturning(emptyGemCollection());
    }
  };

  return (
    <div className="p-3 bg-orange-50 border border-orange-200 rounded-lg space-y-2">
      <h3 className="text-sm font-semibold text-orange-800">
        Return {amount} gem{amount > 1 ? 's' : ''}
      </h3>
      <p className="text-xs text-orange-600">
        You have too many tokens. Select {amount} to return.
      </p>

      <div className="flex gap-2 flex-wrap">
        {ALL_TOKEN_TYPES.map((token) => {
          const playerHas = playerTokens[token] ?? 0;
          const ret = returning[token] ?? 0;
          if (playerHas === 0) return null;
          return (
            <div key={token} className="flex flex-col items-center gap-1">
              <GemToken
                token={token}
                count={playerHas - ret}
                onClick={ret < playerHas && returnedTotal < amount ? () => handleGemClick(token) : undefined}
              />
              {ret > 0 && (
                <button
                  onClick={() => handleUndo(token)}
                  className="text-xs text-orange-700 underline"
                >
                  -{ret}
                </button>
              )}
            </div>
          );
        })}
      </div>

      <div className="flex gap-2">
        <button
          onClick={handleSubmit}
          disabled={!isValid}
          className="px-3 py-1 bg-orange-600 text-white text-sm rounded hover:bg-orange-700 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          Return ({returnedTotal}/{amount})
        </button>
        <button
          onClick={() => setReturning(emptyGemCollection())}
          className="px-3 py-1 border border-gray-300 text-sm rounded hover:bg-gray-100"
        >
          Reset
        </button>
      </div>
    </div>
  );
}
