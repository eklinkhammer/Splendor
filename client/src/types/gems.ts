import type { GemCollection, GemColor, TokenType } from './game';
import { ALL_GEM_COLORS, ALL_TOKEN_TYPES } from './game';

export function gemCount(gc: GemCollection, color: GemColor): number {
  return gc[color] ?? 0;
}

export function goldCount(gc: GemCollection): number {
  return gc['Gold'] ?? 0;
}

export function totalGems(gc: GemCollection): number {
  return ALL_TOKEN_TYPES.reduce((sum, t) => sum + (gc[t] ?? 0), 0);
}

export function tokenCount(gc: GemCollection, token: TokenType): number {
  return gc[token] ?? 0;
}

export function buildGemCollection(entries: [TokenType, number][]): GemCollection {
  const gc: GemCollection = {};
  for (const [token, count] of entries) {
    if (count > 0) {
      gc[token] = count;
    }
  }
  return gc;
}

export function emptyGemCollection(): GemCollection {
  return {};
}

export function addToCollection(gc: GemCollection, token: TokenType, amount: number): GemCollection {
  const current = gc[token] ?? 0;
  const result = { ...gc };
  const newVal = current + amount;
  if (newVal > 0) {
    result[token] = newVal;
  } else {
    delete result[token];
  }
  return result;
}

/** Get bonus counts from a player's purchased cards */
export function countBonuses(purchased: { cardBonus: GemColor }[]): Partial<Record<GemColor, number>> {
  const bonuses: Partial<Record<GemColor, number>> = {};
  for (const card of purchased) {
    bonuses[card.cardBonus] = (bonuses[card.cardBonus] ?? 0) + 1;
  }
  return bonuses;
}

/** Convert GemCollection to display entries (only non-zero) */
export function toDisplayEntries(gc: GemCollection): [TokenType, number][] {
  return ALL_TOKEN_TYPES
    .filter(t => (gc[t] ?? 0) > 0)
    .map(t => [t, gc[t]!]);
}

/** Compute total gems discounted by card bonuses */
export function computeDiscountTotal(
  cardCost: GemCollection,
  bonuses: Partial<Record<GemColor, number>>,
): number {
  return ALL_GEM_COLORS.reduce((sum, c) => {
    const cost = cardCost[c] ?? 0;
    const bonus = bonuses[c] ?? 0;
    return sum + Math.min(cost, bonus);
  }, 0);
}

/** Display entries for gem colors only (no gold) */
export function toGemColorEntries(gc: GemCollection): [GemColor, number][] {
  return ALL_GEM_COLORS
    .filter(c => (gc[c] ?? 0) > 0)
    .map(c => [c, gc[c]!]);
}
