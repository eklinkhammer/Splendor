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
    <div className="bg-gradient-to-b from-orange-900/60 to-orange-950/60 border border-orange-500/40 rounded-xl p-4 shadow-lg space-y-3">
      <div className="flex items-center gap-2">
        <span className="flex items-center justify-center w-5 h-5 bg-orange-500/30 rounded-full text-xs font-bold text-orange-300">!</span>
        <h3 className="text-sm font-bold text-orange-300">
          Return {amount} gem{amount > 1 ? 's' : ''}
        </h3>
        <span className="px-2 py-0.5 text-[10px] font-semibold uppercase tracking-wider bg-orange-500/20 text-orange-300 rounded-full border border-orange-500/30">Required</span>
      </div>
      <p className="text-xs text-orange-200/80">
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
                  className="text-xs text-orange-400 hover:text-orange-300 transition-colors duration-200"
                >
                  undo ({ret})
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
          className="px-4 py-1.5 bg-gradient-to-r from-orange-500 to-orange-600 text-white text-sm rounded-xl font-semibold shadow-md transition-all duration-200 hover:from-orange-600 hover:to-orange-700 hover:shadow-lg disabled:opacity-40 disabled:cursor-not-allowed"
        >
          Return ({returnedTotal}/{amount})
        </button>
        <button
          onClick={() => setReturning(emptyGemCollection())}
          className="px-4 py-1.5 border border-gray-500/50 text-gray-300 text-sm rounded-xl font-medium transition-all duration-200 hover:bg-gray-600/50"
        >
          Reset
        </button>
      </div>
    </div>
  );
}
