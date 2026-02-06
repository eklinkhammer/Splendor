import type { Action, CardId, ClientMessage, PublicGameView } from '../../types';
import { CardDisplay } from '../game-board/CardDisplay';

interface Props {
  legalActions: Action[];
  gameView: PublicGameView;
  selfPlayerId: string | null;
  selectedCardId: CardId | null;
  send: (msg: ClientMessage) => void;
  onClearSelection: () => void;
}

export function BuyCardPanel({ legalActions, gameView, selfPlayerId, selectedCardId, send, onClearSelection }: Props) {
  const buyActions = legalActions.filter(
    (a): a is Extract<Action, { tag: 'BuyCard' }> => a.tag === 'BuyCard',
  );

  if (buyActions.length === 0) return null;

  // Find the action for the selected card
  const selectedAction = selectedCardId
    ? buyActions.find((a) => {
        const [source] = a.contents;
        return (
          (source.tag === 'FromDisplay' || source.tag === 'FromReserve') &&
          source.contents === selectedCardId
        );
      })
    : null;

  // Find the card object
  const findCard = (cardId: CardId) => {
    for (const row of [gameView.pgvBoard.publicTier1, gameView.pgvBoard.publicTier2, gameView.pgvBoard.publicTier3]) {
      const found = row.publicDisplay.find((c) => c.cardId === cardId);
      if (found) return found;
    }
    const self = gameView.pgvPlayers.find((p) => p.ppPlayerId === selfPlayerId);
    if (self?.ppReserved) {
      const found = self.ppReserved.find((c) => c.cardId === cardId);
      if (found) return found;
    }
    return null;
  };

  const selectedCard = selectedCardId ? findCard(selectedCardId) : null;

  const handleBuy = () => {
    if (selectedAction) {
      send({ tag: 'SubmitAction', contents: selectedAction });
      onClearSelection();
    }
  };

  return (
    <div className="space-y-2">
      <h3 className="text-sm font-semibold text-gray-700">Buy a Card</h3>

      {selectedCard && selectedAction ? (
        <div className="flex items-center gap-3 p-2 bg-blue-50 rounded">
          <CardDisplay card={selectedCard} />
          <div className="flex-1">
            <p className="text-sm font-medium">Buy this card?</p>
            <div className="flex gap-2 mt-2">
              <button
                onClick={handleBuy}
                className="px-3 py-1 bg-blue-600 text-white text-sm rounded hover:bg-blue-700"
              >
                Buy
              </button>
              <button
                onClick={onClearSelection}
                className="px-3 py-1 border border-gray-300 text-sm rounded hover:bg-gray-100"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      ) : (
        <p className="text-xs text-gray-500">Click a highlighted card to buy it.</p>
      )}
    </div>
  );
}

/** Get IDs of cards that can be bought from legal actions */
export function getBuyableCardIds(legalActions: Action[]): Set<string> {
  const ids = new Set<string>();
  for (const a of legalActions) {
    if (a.tag === 'BuyCard') {
      const [source] = a.contents;
      if (source.tag === 'FromDisplay' || source.tag === 'FromReserve') {
        ids.add(source.contents);
      }
    }
  }
  return ids;
}
