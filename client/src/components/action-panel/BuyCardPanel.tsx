import type { Action, Card, CardId, ClientMessage, GemCollection, PublicGameView } from '../../types';
import { countBonuses, toDisplayEntries, computeDiscountTotal } from '../../types';
import { CardDisplay } from '../game-board/CardDisplay';
import { GemToken } from '../game-board/GemToken';

export function PaymentBreakdown({
  payment,
  card,
  gameView,
  selfPlayerId,
}: {
  payment: GemCollection;
  card: Card;
  gameView: PublicGameView;
  selfPlayerId: string | null;
}) {
  const paymentEntries = toDisplayEntries(payment);
  if (paymentEntries.length === 0) return null;

  const self = gameView.pgvPlayers.find((p) => p.ppPlayerId === selfPlayerId);
  const bonuses = self ? countBonuses(self.ppPurchased) : {};
  const discountTotal = computeDiscountTotal(card.cardCost, bonuses);
  const hasDiscount = discountTotal > 0;

  return (
    <div className="mt-2 space-y-1.5">
      <div className="flex items-center gap-1.5">
        <span className="text-[10px] font-semibold text-gray-400 uppercase w-14">Paying</span>
        <div className="flex gap-1">
          {paymentEntries.map(([token, count]) => (
            <GemToken key={token} token={token} count={count} size="sm" />
          ))}
        </div>
      </div>
      {hasDiscount && (
        <div className="text-[10px] text-emerald-400">
          {discountTotal} gem{discountTotal > 1 ? 's' : ''} discounted by bonuses
        </div>
      )}
    </div>
  );
}

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
    <div className="bg-gray-700/50 rounded-xl p-3 space-y-2">
      <div className="flex items-center gap-2">
        <span className="text-xs font-semibold uppercase tracking-wider text-blue-400">Buy a Card</span>
        <span className="flex-1 border-t border-white/10" />
      </div>

      {selectedCard && selectedAction ? (
        <div className="flex items-center gap-3 p-2 bg-blue-900/40 border border-blue-500/30 rounded-xl">
          <CardDisplay card={selectedCard} />
          <div className="flex-1">
            <p className="text-sm font-medium text-gray-100">Buy this card?</p>
            <PaymentBreakdown
              payment={selectedAction.contents[1]}
              card={selectedCard}
              gameView={gameView}
              selfPlayerId={selfPlayerId}
            />
            <div className="flex gap-2 mt-2">
              <button
                onClick={handleBuy}
                className="px-4 py-1.5 bg-gradient-to-r from-blue-600 to-blue-700 text-white text-sm rounded-xl font-semibold shadow-md transition-all duration-200 hover:from-blue-700 hover:to-blue-800 hover:shadow-lg"
              >
                Buy
              </button>
              <button
                onClick={onClearSelection}
                className="px-4 py-1.5 border border-gray-500/50 text-gray-300 text-sm rounded-xl font-medium transition-all duration-200 hover:bg-gray-600/50"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      ) : (
        <p className="text-xs text-gray-400">Click a highlighted card to buy it.</p>
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
