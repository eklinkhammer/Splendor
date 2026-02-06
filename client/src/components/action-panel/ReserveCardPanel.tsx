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
    <div className="space-y-2">
      <h3 className="text-sm font-semibold text-gray-700">Reserve a Card</h3>

      {displayReserveAction && selectedCardId && (
        <div className="flex gap-2 items-center">
          <button
            onClick={handleReserveDisplay}
            className="px-3 py-1 bg-yellow-500 text-white text-sm rounded hover:bg-yellow-600"
          >
            Reserve Selected Card
          </button>
          <button
            onClick={onClearSelection}
            className="px-3 py-1 border border-gray-300 text-sm rounded hover:bg-gray-100"
          >
            Cancel
          </button>
        </div>
      )}

      {deckReserveActions.length > 0 && (
        <div>
          <p className="text-xs text-gray-500 mb-1">Reserve from deck:</p>
          <div className="flex gap-2">
            {deckReserveActions.map((a) => (
              <button
                key={a.contents.contents}
                onClick={() => handleReserveDeck(a.contents.contents)}
                className="px-2 py-1 text-xs border rounded hover:bg-gray-100"
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
