import { useState } from 'react';
import type { Action, GemColor, ClientMessage, PublicBoard } from '../../types';
import { ALL_GEM_COLORS } from '../../types';
import { GemToken } from '../game-board/GemToken';

interface Props {
  legalActions: Action[];
  board: PublicBoard;
  send: (msg: ClientMessage) => void;
}

export function TakeGemsPanel({ legalActions, board, send }: Props) {
  const [selectedGems, setSelectedGems] = useState<GemColor[]>([]);

  // Find available take-different actions
  const takeDiffActions = legalActions.filter(
    (a): a is Extract<Action, { tag: 'TakeGems' }> =>
      a.tag === 'TakeGems' && a.contents.tag === 'TakeDifferent',
  );

  // Find take-two-same actions
  const takeTwoActions = legalActions.filter(
    (a): a is Extract<Action, { tag: 'TakeGems' }> =>
      a.tag === 'TakeGems' && a.contents.tag === 'TakeTwoSame',
  );

  // Colors available in any take-different action
  const availableDiffColors = new Set<GemColor>();
  for (const action of takeDiffActions) {
    if (action.contents.tag === 'TakeDifferent') {
      for (const c of action.contents.contents) {
        availableDiffColors.add(c);
      }
    }
  }

  // Colors available for take-two
  const availableTwoColors = new Set<GemColor>();
  for (const action of takeTwoActions) {
    if (action.contents.tag === 'TakeTwoSame') {
      availableTwoColors.add(action.contents.contents);
    }
  }

  const handleGemClick = (color: GemColor) => {
    if (selectedGems.includes(color)) {
      setSelectedGems(selectedGems.filter((c) => c !== color));
    } else {
      setSelectedGems([...selectedGems, color]);
    }
  };

  const handleTakeDifferent = () => {
    // Find the exact matching action
    const match = takeDiffActions.find((a) => {
      if (a.contents.tag !== 'TakeDifferent') return false;
      const gems = a.contents.contents;
      return (
        gems.length === selectedGems.length &&
        selectedGems.every((c) => gems.includes(c))
      );
    });
    if (match) {
      send({ tag: 'SubmitAction', contents: match });
      setSelectedGems([]);
    }
  };

  const handleTakeTwo = (color: GemColor) => {
    const match = takeTwoActions.find(
      (a) => a.contents.tag === 'TakeTwoSame' && a.contents.contents === color,
    );
    if (match) {
      send({ tag: 'SubmitAction', contents: match });
    }
  };

  // Check if current selection is a valid action
  const isValidSelection = takeDiffActions.some((a) => {
    if (a.contents.tag !== 'TakeDifferent') return false;
    const gems = a.contents.contents;
    return (
      gems.length === selectedGems.length &&
      selectedGems.every((c) => gems.includes(c))
    );
  });

  if (takeDiffActions.length === 0 && takeTwoActions.length === 0) return null;

  return (
    <div className="space-y-3">
      <h3 className="text-sm font-semibold text-gray-700">Take Gems</h3>

      {/* Take different */}
      {takeDiffActions.length > 0 && (
        <div>
          <p className="text-xs text-gray-500 mb-1">Select different colors (up to 3):</p>
          <div className="flex gap-2 items-center">
            {ALL_GEM_COLORS.map((color) => {
              const bankCount = board.publicBank[color] ?? 0;
              const available = availableDiffColors.has(color);
              return (
                <GemToken
                  key={color}
                  token={color}
                  count={bankCount}
                  onClick={available ? () => handleGemClick(color) : undefined}
                  selected={selectedGems.includes(color)}
                  size="sm"
                />
              );
            })}
            <button
              onClick={handleTakeDifferent}
              disabled={!isValidSelection}
              className="ml-2 px-3 py-1 bg-blue-600 text-white text-sm rounded hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Take
            </button>
          </div>
        </div>
      )}

      {/* Take two same */}
      {takeTwoActions.length > 0 && (
        <div>
          <p className="text-xs text-gray-500 mb-1">Take 2 of same color:</p>
          <div className="flex gap-2">
            {ALL_GEM_COLORS.filter((c) => availableTwoColors.has(c)).map((color) => (
              <button
                key={color}
                onClick={() => handleTakeTwo(color)}
                className="px-2 py-1 text-xs border rounded hover:bg-gray-100"
              >
                2x
                <GemToken token={color} count={board.publicBank[color] ?? 0} size="sm" />
              </button>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}
