import { describe, it, expect } from 'vitest';
import type { GemCollection, GemColor } from './game';
import {
  gemCount,
  goldCount,
  totalGems,
  tokenCount,
  buildGemCollection,
  emptyGemCollection,
  addToCollection,
  countBonuses,
  toDisplayEntries,
  toGemColorEntries,
  computeDiscountTotal,
} from './gems';

describe('gemCount', () => {
  it('returns count for a present color', () => {
    expect(gemCount({ Diamond: 3, Ruby: 1 }, 'Diamond')).toBe(3);
  });

  it('returns 0 for an absent color', () => {
    expect(gemCount({ Diamond: 3 }, 'Sapphire')).toBe(0);
  });

  it('returns 0 for empty collection', () => {
    expect(gemCount({}, 'Emerald')).toBe(0);
  });
});

describe('goldCount', () => {
  it('returns gold when present', () => {
    expect(goldCount({ Gold: 2, Diamond: 1 })).toBe(2);
  });

  it('returns 0 when gold absent', () => {
    expect(goldCount({ Diamond: 1 })).toBe(0);
  });

  it('returns 0 for empty collection', () => {
    expect(goldCount({})).toBe(0);
  });
});

describe('totalGems', () => {
  it('sums all token types', () => {
    const gc: GemCollection = { Diamond: 1, Sapphire: 2, Emerald: 3, Ruby: 4, Onyx: 5, Gold: 6 };
    expect(totalGems(gc)).toBe(21);
  });

  it('returns 0 for empty collection', () => {
    expect(totalGems({})).toBe(0);
  });

  it('handles partial collection', () => {
    expect(totalGems({ Ruby: 3, Gold: 1 })).toBe(4);
  });
});

describe('tokenCount', () => {
  it('returns count for Gold token', () => {
    expect(tokenCount({ Gold: 5 }, 'Gold')).toBe(5);
  });

  it('returns count for gem color token', () => {
    expect(tokenCount({ Onyx: 3 }, 'Onyx')).toBe(3);
  });

  it('returns 0 for absent token', () => {
    expect(tokenCount({ Diamond: 1 }, 'Gold')).toBe(0);
  });
});

describe('buildGemCollection', () => {
  it('builds from normal entries', () => {
    const gc = buildGemCollection([['Diamond', 3], ['Ruby', 1]]);
    expect(gc).toEqual({ Diamond: 3, Ruby: 1 });
  });

  it('omits zero-count entries', () => {
    const gc = buildGemCollection([['Diamond', 0], ['Ruby', 2]]);
    expect(gc).toEqual({ Ruby: 2 });
  });

  it('omits negative-count entries', () => {
    const gc = buildGemCollection([['Diamond', -1], ['Ruby', 2]]);
    expect(gc).toEqual({ Ruby: 2 });
  });

  it('returns empty object for empty input', () => {
    expect(buildGemCollection([])).toEqual({});
  });
});

describe('emptyGemCollection', () => {
  it('returns empty object', () => {
    expect(emptyGemCollection()).toEqual({});
  });

  it('returns a new reference each call', () => {
    const a = emptyGemCollection();
    const b = emptyGemCollection();
    expect(a).not.toBe(b);
  });
});

describe('addToCollection', () => {
  it('adds to an existing type', () => {
    const result = addToCollection({ Diamond: 2 }, 'Diamond', 3);
    expect(result).toEqual({ Diamond: 5 });
  });

  it('adds a new type', () => {
    const result = addToCollection({ Diamond: 2 }, 'Ruby', 1);
    expect(result).toEqual({ Diamond: 2, Ruby: 1 });
  });

  it('removes key when result is 0', () => {
    const result = addToCollection({ Diamond: 2 }, 'Diamond', -2);
    expect(result).toEqual({});
    expect('Diamond' in result).toBe(false);
  });

  it('removes key when result is negative', () => {
    const result = addToCollection({ Diamond: 1 }, 'Diamond', -5);
    expect(result).toEqual({});
    expect('Diamond' in result).toBe(false);
  });

  it('does not mutate the original', () => {
    const original: GemCollection = { Diamond: 2 };
    addToCollection(original, 'Diamond', 3);
    expect(original).toEqual({ Diamond: 2 });
  });
});

describe('countBonuses', () => {
  it('counts bonuses from multiple cards', () => {
    const cards = [
      { cardBonus: 'Diamond' as GemColor },
      { cardBonus: 'Diamond' as GemColor },
      { cardBonus: 'Ruby' as GemColor },
    ];
    expect(countBonuses(cards)).toEqual({ Diamond: 2, Ruby: 1 });
  });

  it('returns empty for no cards', () => {
    expect(countBonuses([])).toEqual({});
  });

  it('handles single card', () => {
    expect(countBonuses([{ cardBonus: 'Emerald' }])).toEqual({ Emerald: 1 });
  });
});

describe('toDisplayEntries', () => {
  it('returns non-zero entries in ALL_TOKEN_TYPES order', () => {
    const gc: GemCollection = { Ruby: 2, Diamond: 1, Gold: 3 };
    const entries = toDisplayEntries(gc);
    expect(entries).toEqual([
      ['Diamond', 1],
      ['Ruby', 2],
      ['Gold', 3],
    ]);
  });

  it('omits zero-value entries', () => {
    // Explicitly setting to 0 shouldn't appear
    const gc: GemCollection = { Diamond: 0, Ruby: 2 } as GemCollection;
    const entries = toDisplayEntries(gc);
    expect(entries).toEqual([['Ruby', 2]]);
  });

  it('returns empty array for empty collection', () => {
    expect(toDisplayEntries({})).toEqual([]);
  });
});

describe('toGemColorEntries', () => {
  it('excludes Gold', () => {
    const gc: GemCollection = { Diamond: 1, Gold: 5 };
    const entries = toGemColorEntries(gc);
    expect(entries).toEqual([['Diamond', 1]]);
  });

  it('preserves ALL_GEM_COLORS order', () => {
    const gc: GemCollection = { Onyx: 1, Diamond: 2, Emerald: 3 };
    const entries = toGemColorEntries(gc);
    expect(entries).toEqual([
      ['Diamond', 2],
      ['Emerald', 3],
      ['Onyx', 1],
    ]);
  });

  it('returns empty for empty collection', () => {
    expect(toGemColorEntries({})).toEqual([]);
  });

  it('returns empty for gold-only collection', () => {
    expect(toGemColorEntries({ Gold: 5 })).toEqual([]);
  });
});

describe('computeDiscountTotal', () => {
  it('returns 0 when no bonuses match', () => {
    expect(computeDiscountTotal({ Ruby: 3 }, {})).toBe(0);
  });

  it('caps discount at card cost', () => {
    expect(computeDiscountTotal({ Diamond: 2 }, { Diamond: 3 })).toBe(2);
  });

  it('returns partial discount when bonus < cost', () => {
    expect(computeDiscountTotal({ Ruby: 4 }, { Ruby: 2 })).toBe(2);
  });

  it('sums discounts across multiple colors', () => {
    expect(computeDiscountTotal({ Diamond: 2, Ruby: 3 }, { Diamond: 1, Ruby: 2 })).toBe(3);
  });

  it('returns 0 when colors do not overlap', () => {
    expect(computeDiscountTotal({ Diamond: 3 }, { Ruby: 2 })).toBe(0);
  });

  it('returns 0 for empty card cost', () => {
    expect(computeDiscountTotal({}, { Ruby: 2, Diamond: 1 })).toBe(0);
  });

  it('returns 0 for empty bonuses', () => {
    expect(computeDiscountTotal({ Ruby: 3, Emerald: 2 }, {})).toBe(0);
  });
});
