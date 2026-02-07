import { describe, it, expect } from 'vitest';
import type { PublicGameView, PublicPlayer, Card } from '../types';
import { computeMoveLogEntry } from './moveLog';

function makePlayer(overrides?: Partial<PublicPlayer>): PublicPlayer {
  return {
    ppPlayerId: 'player-1',
    ppPlayerName: 'Alice',
    ppTokens: {},
    ppPurchased: [],
    ppReservedCount: 0,
    ppReserved: null,
    ppNobles: [],
    ppPrestige: 0,
    ...overrides,
  };
}

function makeGameView(overrides?: Partial<PublicGameView>): PublicGameView {
  const emptyRow = { publicDeckCount: 0, publicDisplay: [] };
  return {
    pgvGameId: 'game-1',
    pgvBoard: {
      publicTier1: emptyRow,
      publicTier2: emptyRow,
      publicTier3: emptyRow,
      publicNobles: [],
      publicBank: {},
    },
    pgvPlayers: [makePlayer()],
    pgvCurrentPlayer: 0,
    pgvTurnNumber: 1,
    pgvPhase: { tag: 'InProgress' },
    pgvTurnPhase: { tag: 'AwaitingAction' },
    ...overrides,
  };
}

function makeCard(overrides?: Partial<Card>): Card {
  return {
    cardId: 'c1',
    cardTier: 'Tier1',
    cardCost: {},
    cardBonus: 'Diamond',
    cardPrestige: 0,
    ...overrides,
  };
}

describe('computeMoveLogEntry', () => {
  it('detects a card purchase', () => {
    const card = makeCard({ cardBonus: 'Diamond', cardPrestige: 0 });
    const prev = makeGameView({
      pgvPlayers: [makePlayer({ ppPurchased: [] })],
      pgvCurrentPlayer: 0,
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer({ ppPurchased: [card] })],
      pgvCurrentPlayer: 1,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry).not.toBeNull();
    expect(entry!.description).toBe('Bought a Diamond card');
  });

  it('includes prestige in card purchase description', () => {
    const card = makeCard({ cardBonus: 'Ruby', cardPrestige: 3 });
    const prev = makeGameView({
      pgvPlayers: [makePlayer({ ppPurchased: [] })],
      pgvCurrentPlayer: 0,
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer({ ppPurchased: [card] })],
      pgvCurrentPlayer: 1,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry!.description).toBe('Bought a Ruby card (3 VP)');
  });

  it('detects reserve card with gold', () => {
    const prev = makeGameView({
      pgvPlayers: [makePlayer({ ppReservedCount: 0, ppTokens: {} })],
      pgvCurrentPlayer: 0,
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer({ ppReservedCount: 1, ppTokens: { Gold: 1 } })],
      pgvCurrentPlayer: 1,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry!.description).toBe('Reserved a card (+1 Gold)');
  });

  it('detects reserve card without gold', () => {
    const prev = makeGameView({
      pgvPlayers: [makePlayer({ ppReservedCount: 0, ppTokens: {} })],
      pgvCurrentPlayer: 0,
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer({ ppReservedCount: 1, ppTokens: {} })],
      pgvCurrentPlayer: 1,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry!.description).toBe('Reserved a card');
  });

  it('detects taking 3 different gems', () => {
    const prev = makeGameView({
      pgvPlayers: [makePlayer({ ppTokens: {} })],
      pgvCurrentPlayer: 0,
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer({ ppTokens: { Diamond: 1, Sapphire: 1, Emerald: 1 } })],
      pgvCurrentPlayer: 1,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry!.description).toBe('Took gems: Diamond, Sapphire, Emerald');
  });

  it('detects taking 2 same gems', () => {
    const prev = makeGameView({
      pgvPlayers: [makePlayer({ ppTokens: {} })],
      pgvCurrentPlayer: 0,
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer({ ppTokens: { Ruby: 2 } })],
      pgvCurrentPlayer: 1,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry!.description).toBe('Took 2 Ruby');
  });

  it('appends noble earned suffix', () => {
    const card = makeCard({ cardBonus: 'Onyx', cardPrestige: 1 });
    const noble = { nobleId: 'n1', nobleRequirement: { Onyx: 3 }, noblePrestige: 3 };
    const prev = makeGameView({
      pgvPlayers: [makePlayer({ ppPurchased: [], ppNobles: [] })],
      pgvCurrentPlayer: 0,
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer({ ppPurchased: [card], ppNobles: [noble] })],
      pgvCurrentPlayer: 1,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry!.description).toContain('— earned a Noble!');
    // Should start with the buy description
    expect(entry!.description).toMatch(/^Bought a Onyx card.*— earned a Noble!$/);
  });

  it('returns null for invalid player index', () => {
    const prev = makeGameView({
      pgvPlayers: [makePlayer()],
      pgvCurrentPlayer: 5, // out of bounds
    });
    const curr = makeGameView({
      pgvPlayers: [makePlayer()],
      pgvCurrentPlayer: 0,
      pgvTurnNumber: 2,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry).toBeNull();
  });

  it('sets correct metadata fields', () => {
    const prev = makeGameView({
      pgvPlayers: [
        makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Bob', ppTokens: {} }),
      ],
      pgvCurrentPlayer: 0,
      pgvTurnNumber: 3,
    });
    const curr = makeGameView({
      pgvPlayers: [
        makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Bob', ppTokens: { Diamond: 1, Ruby: 1, Emerald: 1 } }),
      ],
      pgvCurrentPlayer: 0,
      pgvTurnNumber: 4,
    });

    const entry = computeMoveLogEntry(prev, curr);
    expect(entry).not.toBeNull();
    expect(entry!.turnNumber).toBe(3);
    expect(entry!.playerId).toBe('p1');
    expect(entry!.playerName).toBe('Bob');
  });
});
