import type { Action, CardId, Tier, ClientMessage } from '../../types';

interface Props {
  legalActions: Action[];
  selectedCardId: CardId | null;
  send: (msg: ClientMessage) => void;
  onClearSelection: () => void;
}

export function ReserveCardPanel({ legalActions, selectedCardId, send, onClearSelection }: Props) {
  const reserveActions = legalActions.filter(
    (a): a is Extract<Action, { tag: 'ReserveCard' }> => a.tag === 'ReserveCard',
  );

  if (reserveActions.length === 0) return null;

  // Can reserve from display
  const displayReserveAction = selectedCardId
    ? reserveActions.find(
        (a) => a.contents.tag === 'FromDisplay' && a.contents.contents === selectedCardId,
      )
    : null;

  // Can reserve from deck (top of deck)
  const deckReserveActions = reserveActions.filter(
    (a): a is Extract<Action, { tag: 'ReserveCard' }> & { contents: { tag: 'FromTopOfDeck'; contents: Tier } } =>
      a.contents.tag === 'FromTopOfDeck',
  );

  const handleReserveDisplay = () => {
    if (displayReserveAction) {
      send({ tag: 'SubmitAction', contents: displayReserveAction });
      onClearSelection();
    }
  };

  const handleReserveDeck = (tier: Tier) => {
    const action = deckReserveActions.find((a) => a.contents.contents === tier);
    if (action) {
      send({ tag: 'SubmitAction', contents: action });
    }
  };

  return (
    <div className="bg-gray-700/50 rounded-xl p-3 space-y-2">
      <div className="flex items-center gap-2">
        <span className="text-xs font-semibold uppercase tracking-wider text-amber-400">Reserve a Card</span>
        <span className="flex-1 border-t border-white/10" />
      </div>

      {displayReserveAction && selectedCardId && (
        <div className="flex gap-2 items-center">
          <button
            onClick={handleReserveDisplay}
            className="px-4 py-1.5 bg-gradient-to-r from-amber-500 to-yellow-600 text-white text-sm rounded-xl font-semibold shadow-md transition-all duration-200 hover:from-amber-600 hover:to-yellow-700 hover:shadow-lg"
          >
            Reserve Selected Card
          </button>
          <button
            onClick={onClearSelection}
            className="px-4 py-1.5 border border-gray-500/50 text-gray-300 text-sm rounded-xl font-medium transition-all duration-200 hover:bg-gray-600/50"
          >
            Cancel
          </button>
        </div>
      )}

      {deckReserveActions.length > 0 && (
        <div>
          <p className="text-xs text-gray-400 mb-1">Reserve from deck:</p>
          <div className="flex gap-2">
            {deckReserveActions.map((a) => (
              <button
                key={a.contents.contents}
                onClick={() => handleReserveDeck(a.contents.contents)}
                className="px-2 py-1 text-xs bg-gray-600/80 border border-gray-500/50 rounded-xl text-gray-200 transition-all duration-200 hover:bg-gray-500/80 hover:shadow-lg"
              >
                {a.contents.contents.replace('Tier', 'Tier ')}
              </button>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

/** Get IDs of cards that can be reserved from legal actions */
export function getReservableCardIds(legalActions: Action[]): Set<string> {
  const ids = new Set<string>();
  for (const a of legalActions) {
    if (a.tag === 'ReserveCard' && a.contents.tag === 'FromDisplay') {
      ids.add(a.contents.contents);
    }
  }
  return ids;
}
