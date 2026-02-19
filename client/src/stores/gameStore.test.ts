import { describe, it, expect, beforeEach } from 'vitest';
import type { PublicGameView, PublicPlayer, Action, Noble, GameResult, GemCollection } from '../types';
import { useGameStore } from './gameStore';

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
    ppIsAI: false,
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

describe('gameStore', () => {
  beforeEach(() => {
    useGameStore.getState().reset();
  });

  describe('initial state', () => {
    it('has null gameView', () => {
      expect(useGameStore.getState().gameView).toBeNull();
    });

    it('has empty legalActions', () => {
      expect(useGameStore.getState().legalActions).toEqual([]);
    });

    it('has null gemReturnInfo', () => {
      expect(useGameStore.getState().gemReturnInfo).toBeNull();
    });

    it('has null nobleChoices', () => {
      expect(useGameStore.getState().nobleChoices).toBeNull();
    });

    it('has null gameResult', () => {
      expect(useGameStore.getState().gameResult).toBeNull();
    });

    it('has null error', () => {
      expect(useGameStore.getState().error).toBeNull();
    });

    it('has connected false', () => {
      expect(useGameStore.getState().connected).toBe(false);
    });

    it('has null selfPlayerId', () => {
      expect(useGameStore.getState().selfPlayerId).toBeNull();
    });
  });

  describe('setConnected', () => {
    it('sets connected to true', () => {
      useGameStore.getState().setConnected(true);
      expect(useGameStore.getState().connected).toBe(true);
    });

    it('sets connected to false', () => {
      useGameStore.getState().setConnected(true);
      useGameStore.getState().setConnected(false);
      expect(useGameStore.getState().connected).toBe(false);
    });
  });

  describe('clearError', () => {
    it('clears an existing error', () => {
      useGameStore.getState().handleServerMessage({ tag: 'ErrorMsg', contents: 'oops' });
      expect(useGameStore.getState().error).toBe('oops');
      useGameStore.getState().clearError();
      expect(useGameStore.getState().error).toBeNull();
    });

    it('is a no-op when error is null', () => {
      useGameStore.getState().clearError();
      expect(useGameStore.getState().error).toBeNull();
    });
  });

  describe('reset', () => {
    it('restores all fields after mutations', () => {
      const { handleServerMessage, setConnected } = useGameStore.getState();
      setConnected(true);
      handleServerMessage({
        tag: 'GameStateUpdate',
        contents: makeGameView({ pgvPlayers: [makePlayer({ ppReserved: [] })] }),
      });
      handleServerMessage({ tag: 'ErrorMsg', contents: 'err' });

      useGameStore.getState().reset();

      const s = useGameStore.getState();
      expect(s.gameView).toBeNull();
      expect(s.legalActions).toEqual([]);
      expect(s.gemReturnInfo).toBeNull();
      expect(s.nobleChoices).toBeNull();
      expect(s.gameResult).toBeNull();
      expect(s.error).toBeNull();
      expect(s.connected).toBe(false);
      expect(s.selfPlayerId).toBeNull();
    });
  });

  describe('GameStateUpdate', () => {
    it('sets gameView', () => {
      const view = makeGameView();
      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: view });
      expect(useGameStore.getState().gameView).toBe(view);
    });

    it('derives selfPlayerId from player with ppReserved !== null', () => {
      const self = makePlayer({ ppPlayerId: 'me', ppReserved: [] });
      const opponent = makePlayer({ ppPlayerId: 'them', ppReserved: null });
      const view = makeGameView({ pgvPlayers: [opponent, self] });

      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: view });
      expect(useGameStore.getState().selfPlayerId).toBe('me');
    });

    it('does not overwrite selfPlayerId once set', () => {
      const self = makePlayer({ ppPlayerId: 'me', ppReserved: [] });
      const view1 = makeGameView({ pgvPlayers: [self] });
      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: view1 });
      expect(useGameStore.getState().selfPlayerId).toBe('me');

      // Second update where no player has ppReserved (edge case)
      const view2 = makeGameView({ pgvPlayers: [makePlayer({ ppPlayerId: 'me', ppReserved: null })] });
      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: view2 });
      expect(useGameStore.getState().selfPlayerId).toBe('me');
    });

    it('clears gemReturnInfo', () => {
      const options: GemCollection[] = [{ Diamond: 1 }];
      useGameStore.getState().handleServerMessage({ tag: 'GemReturnNeeded', contents: [1, options] });
      expect(useGameStore.getState().gemReturnInfo).not.toBeNull();

      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: makeGameView() });
      expect(useGameStore.getState().gemReturnInfo).toBeNull();
    });

    it('clears nobleChoices', () => {
      const noble: Noble = { nobleId: 'n1', nobleRequirement: { Diamond: 3 }, noblePrestige: 3 };
      useGameStore.getState().handleServerMessage({ tag: 'NobleChoiceRequired', contents: [noble] });
      expect(useGameStore.getState().nobleChoices).not.toBeNull();

      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: makeGameView() });
      expect(useGameStore.getState().nobleChoices).toBeNull();
    });

    it('clears error', () => {
      useGameStore.getState().handleServerMessage({ tag: 'ErrorMsg', contents: 'err' });
      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: makeGameView() });
      expect(useGameStore.getState().error).toBeNull();
    });

    it('clears legalActions', () => {
      const actions: Action[] = [
        { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond'] } },
      ];
      useGameStore.getState().handleServerMessage({ tag: 'ActionRequired', contents: actions });
      expect(useGameStore.getState().legalActions).toHaveLength(1);

      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: makeGameView() });
      expect(useGameStore.getState().legalActions).toEqual([]);
    });
  });

  describe('ActionRequired', () => {
    it('sets legalActions', () => {
      const actions: Action[] = [
        { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond', 'Ruby', 'Sapphire'] } },
      ];
      useGameStore.getState().handleServerMessage({ tag: 'ActionRequired', contents: actions });
      expect(useGameStore.getState().legalActions).toBe(actions);
    });

    it('replaces previous legalActions', () => {
      const actions1: Action[] = [
        { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond'] } },
      ];
      const actions2: Action[] = [
        { tag: 'ReserveCard', contents: { tag: 'FromTopOfDeck', contents: 'Tier1' } },
      ];
      useGameStore.getState().handleServerMessage({ tag: 'ActionRequired', contents: actions1 });
      useGameStore.getState().handleServerMessage({ tag: 'ActionRequired', contents: actions2 });
      expect(useGameStore.getState().legalActions).toBe(actions2);
    });
  });

  describe('GemReturnNeeded', () => {
    it('sets gemReturnInfo with amount and options', () => {
      const options: GemCollection[] = [{ Diamond: 1 }, { Ruby: 1 }];
      useGameStore.getState().handleServerMessage({ tag: 'GemReturnNeeded', contents: [2, options] });
      expect(useGameStore.getState().gemReturnInfo).toEqual({ amount: 2, options });
    });

    it('clears legalActions', () => {
      const actions: Action[] = [
        { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond'] } },
      ];
      useGameStore.getState().handleServerMessage({ tag: 'ActionRequired', contents: actions });
      useGameStore.getState().handleServerMessage({ tag: 'GemReturnNeeded', contents: [1, [{ Diamond: 1 }]] });
      expect(useGameStore.getState().legalActions).toEqual([]);
    });
  });

  describe('NobleChoiceRequired', () => {
    it('sets nobleChoices', () => {
      const nobles: Noble[] = [
        { nobleId: 'n1', nobleRequirement: { Diamond: 3 }, noblePrestige: 3 },
        { nobleId: 'n2', nobleRequirement: { Ruby: 3 }, noblePrestige: 3 },
      ];
      useGameStore.getState().handleServerMessage({ tag: 'NobleChoiceRequired', contents: nobles });
      expect(useGameStore.getState().nobleChoices).toBe(nobles);
    });

    it('clears legalActions', () => {
      const actions: Action[] = [
        { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond'] } },
      ];
      useGameStore.getState().handleServerMessage({ tag: 'ActionRequired', contents: actions });

      const nobles: Noble[] = [{ nobleId: 'n1', nobleRequirement: {}, noblePrestige: 3 }];
      useGameStore.getState().handleServerMessage({ tag: 'NobleChoiceRequired', contents: nobles });
      expect(useGameStore.getState().legalActions).toEqual([]);
    });
  });

  describe('GameOverMsg', () => {
    it('sets gameResult', () => {
      const result: GameResult = { winnerId: 'p1', winnerName: 'Alice', finalPrestige: 15 };
      useGameStore.getState().handleServerMessage({ tag: 'GameOverMsg', contents: result });
      expect(useGameStore.getState().gameResult).toBe(result);
    });

    it('clears legalActions', () => {
      const actions: Action[] = [
        { tag: 'TakeGems', contents: { tag: 'TakeDifferent', contents: ['Diamond'] } },
      ];
      useGameStore.getState().handleServerMessage({ tag: 'ActionRequired', contents: actions });

      const result: GameResult = { winnerId: 'p1', winnerName: 'Alice', finalPrestige: 15 };
      useGameStore.getState().handleServerMessage({ tag: 'GameOverMsg', contents: result });
      expect(useGameStore.getState().legalActions).toEqual([]);
    });
  });

  describe('ErrorMsg', () => {
    it('sets error string', () => {
      useGameStore.getState().handleServerMessage({ tag: 'ErrorMsg', contents: 'something broke' });
      expect(useGameStore.getState().error).toBe('something broke');
    });

    it('does not clear other state', () => {
      const view = makeGameView();
      useGameStore.getState().handleServerMessage({ tag: 'GameStateUpdate', contents: view });
      useGameStore.getState().handleServerMessage({ tag: 'ErrorMsg', contents: 'err' });
      expect(useGameStore.getState().gameView).toBe(view);
    });
  });

  describe('Pong', () => {
    it('causes no state change', () => {
      const stateBefore = { ...useGameStore.getState() };
      useGameStore.getState().handleServerMessage({ tag: 'Pong' });
      const stateAfter = useGameStore.getState();

      expect(stateAfter.gameView).toBe(stateBefore.gameView);
      expect(stateAfter.legalActions).toEqual(stateBefore.legalActions);
      expect(stateAfter.gemReturnInfo).toBe(stateBefore.gemReturnInfo);
      expect(stateAfter.nobleChoices).toBe(stateBefore.nobleChoices);
      expect(stateAfter.gameResult).toBe(stateBefore.gameResult);
      expect(stateAfter.error).toBe(stateBefore.error);
      expect(stateAfter.connected).toBe(stateBefore.connected);
      expect(stateAfter.selfPlayerId).toBe(stateBefore.selfPlayerId);
    });
  });

  describe('move log', () => {
    it('moveLog starts empty', () => {
      expect(useGameStore.getState().moveLog).toEqual([]);
    });

    it('lastMove starts null', () => {
      expect(useGameStore.getState().lastMove).toBeNull();
    });

    it('appends to moveLog on turn advance', () => {
      const { handleServerMessage } = useGameStore.getState();

      // Turn 1: player takes gems
      const view1 = makeGameView({
        pgvPlayers: [
          makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Alice', ppTokens: {}, ppReserved: [] }),
        ],
        pgvCurrentPlayer: 0,
        pgvTurnNumber: 1,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view1 });
      expect(useGameStore.getState().moveLog).toEqual([]);

      // Turn 2: player gained gems since turn 1
      const view2 = makeGameView({
        pgvPlayers: [
          makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Alice', ppTokens: { Diamond: 1, Ruby: 1, Sapphire: 1 }, ppReserved: [] }),
        ],
        pgvCurrentPlayer: 0,
        pgvTurnNumber: 2,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view2 });

      const log = useGameStore.getState().moveLog;
      expect(log).toHaveLength(1);
      expect(log[0]!.description).toBe('Took gems: Diamond, Sapphire, Ruby');
    });

    it('does not add entry for same-turn GSU', () => {
      const { handleServerMessage } = useGameStore.getState();

      const view1 = makeGameView({
        pgvPlayers: [makePlayer({ ppReserved: [] })],
        pgvTurnNumber: 1,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view1 });

      // Second GSU at same turn (e.g. gem return phase)
      const view2 = makeGameView({
        pgvPlayers: [makePlayer({ ppTokens: { Diamond: 1 }, ppReserved: [] })],
        pgvTurnNumber: 1,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view2 });

      expect(useGameStore.getState().moveLog).toEqual([]);
    });

    it('updates lastMove on turn advance', () => {
      const { handleServerMessage } = useGameStore.getState();

      const view1 = makeGameView({
        pgvPlayers: [
          makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Alice', ppReservedCount: 0, ppReserved: [] }),
        ],
        pgvCurrentPlayer: 0,
        pgvTurnNumber: 1,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view1 });

      const view2 = makeGameView({
        pgvPlayers: [
          makePlayer({ ppPlayerId: 'p1', ppPlayerName: 'Alice', ppReservedCount: 1, ppTokens: { Gold: 1 }, ppReserved: [] }),
        ],
        pgvCurrentPlayer: 0,
        pgvTurnNumber: 2,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view2 });

      const lastMove = useGameStore.getState().lastMove;
      expect(lastMove).not.toBeNull();
      expect(lastMove!.playerId).toBe('p1');
      expect(lastMove!.description).toBe('Reserved a card (+1 Gold)');
    });

    it('reset clears moveLog and lastMove', () => {
      const { handleServerMessage } = useGameStore.getState();

      // Build up some log state
      const view1 = makeGameView({
        pgvPlayers: [makePlayer({ ppTokens: {}, ppReserved: [] })],
        pgvTurnNumber: 1,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view1 });

      const view2 = makeGameView({
        pgvPlayers: [makePlayer({ ppTokens: { Ruby: 2 }, ppReserved: [] })],
        pgvTurnNumber: 2,
      });
      handleServerMessage({ tag: 'GameStateUpdate', contents: view2 });

      expect(useGameStore.getState().moveLog.length).toBeGreaterThan(0);
      expect(useGameStore.getState().lastMove).not.toBeNull();

      useGameStore.getState().reset();

      expect(useGameStore.getState().moveLog).toEqual([]);
      expect(useGameStore.getState().lastMove).toBeNull();
    });
  });
});
