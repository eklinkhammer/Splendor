import type { Action, Card, GemCollection, PublicGameView } from '../../types';
import { countBonuses, toDisplayEntries, computeDiscountTotal } from '../../types';
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
