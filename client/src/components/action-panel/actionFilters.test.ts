import { describe, it, expect } from 'vitest';
import type { Action } from '../../types';
import { getBuyableCardIds } from './BuyCardPanel';
import { getReservableCardIds } from './ReserveCardPanel';

describe('getBuyableCardIds', () => {
  it('returns empty set for empty actions', () => {
    expect(getBuyableCardIds([])).toEqual(new Set());
  });

  it('returns empty set when no BuyCard actions', () => {
    const actions: Action[] = [
      { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond', 'Ruby', 'Sapphire'] } },
    ];
    expect(getBuyableCardIds(actions)).toEqual(new Set());
  });

  it('extracts card ID from FromDisplay source', () => {
    const actions: Action[] = [
      { tag: 'BuyCard', contents: [{ tag: 'FromDisplay', contents: 'card-1' }, {}] },
    ];
    expect(getBuyableCardIds(actions)).toEqual(new Set(['card-1']));
  });

  it('extracts card ID from FromReserve source', () => {
    const actions: Action[] = [
      { tag: 'BuyCard', contents: [{ tag: 'FromReserve', contents: 'card-r1' }, {}] },
    ];
    expect(getBuyableCardIds(actions)).toEqual(new Set(['card-r1']));
  });

  it('ignores FromTopOfDeck source', () => {
    const actions: Action[] = [
      { tag: 'BuyCard', contents: [{ tag: 'FromTopOfDeck', contents: 'Tier1' }, {}] },
    ];
    expect(getBuyableCardIds(actions)).toEqual(new Set());
  });

  it('ignores non-BuyCard action types', () => {
    const actions: Action[] = [
      { tag: 'ReserveCard', contents: { tag: 'FromDisplay', contents: 'card-1' } },
      { tag: 'BuyCard', contents: [{ tag: 'FromDisplay', contents: 'card-2' }, {}] },
    ];
    expect(getBuyableCardIds(actions)).toEqual(new Set(['card-2']));
  });

  it('deduplicates card IDs', () => {
    const actions: Action[] = [
      { tag: 'BuyCard', contents: [{ tag: 'FromDisplay', contents: 'card-1' }, { Diamond: 2 }] },
      { tag: 'BuyCard', contents: [{ tag: 'FromDisplay', contents: 'card-1' }, { Diamond: 1, Gold: 1 }] },
    ];
    const ids = getBuyableCardIds(actions);
    expect(ids.size).toBe(1);
    expect(ids.has('card-1')).toBe(true);
  });
});

describe('getReservableCardIds', () => {
  it('returns empty set for empty actions', () => {
    expect(getReservableCardIds([])).toEqual(new Set());
  });

  it('returns empty set when no ReserveCard actions', () => {
    const actions: Action[] = [
      { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond'] } },
    ];
    expect(getReservableCardIds(actions)).toEqual(new Set());
  });

  it('extracts card ID from FromDisplay source', () => {
    const actions: Action[] = [
      { tag: 'ReserveCard', contents: { tag: 'FromDisplay', contents: 'card-5' } },
    ];
    expect(getReservableCardIds(actions)).toEqual(new Set(['card-5']));
  });

  it('ignores FromTopOfDeck source', () => {
    const actions: Action[] = [
      { tag: 'ReserveCard', contents: { tag: 'FromTopOfDeck', contents: 'Tier2' } },
    ];
    expect(getReservableCardIds(actions)).toEqual(new Set());
  });

  it('ignores BuyCard actions', () => {
    const actions: Action[] = [
      { tag: 'BuyCard', contents: [{ tag: 'FromDisplay', contents: 'card-1' }, {}] },
      { tag: 'ReserveCard', contents: { tag: 'FromDisplay', contents: 'card-2' } },
    ];
    expect(getReservableCardIds(actions)).toEqual(new Set(['card-2']));
  });

  it('collects multiple reservable IDs', () => {
    const actions: Action[] = [
      { tag: 'ReserveCard', contents: { tag: 'FromDisplay', contents: 'card-a' } },
      { tag: 'ReserveCard', contents: { tag: 'FromDisplay', contents: 'card-b' } },
      { tag: 'ReserveCard', contents: { tag: 'FromTopOfDeck', contents: 'Tier1' } },
    ];
    expect(getReservableCardIds(actions)).toEqual(new Set(['card-a', 'card-b']));
  });
});
