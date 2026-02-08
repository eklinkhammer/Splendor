import type { Action } from '../../types';

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
