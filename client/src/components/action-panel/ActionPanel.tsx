import { useState, useCallback, useMemo } from 'react';
import type { ClientMessage, CardId, Tier, Action, GemColor, GemCollection } from '../../types';
import { useGameStore } from '../../stores/gameStore';
import { totalGems, willRequireGemReturn } from '../../types';

/** Hook for GamePage to get interactive callbacks for the board */
export function useActionCallbacks(
  send: (msg: ClientMessage) => void,
  playerTokens: GemCollection,
  bank: GemCollection,
) {
  const legalActions = useGameStore((s) => s.legalActions);
  const [selectedCardId, setSelectedCardId] = useState<CardId | null>(null);
  const [selectedDeckTier, setSelectedDeckTier] = useState<Tier | null>(null);
  const [selectedBankGems, setSelectedBankGems] = useState<GemColor[]>([]);
  const [pendingAction, setPendingAction] = useState<Action | null>(null);

  const currentTokenTotal = useMemo(() => totalGems(playerTokens), [playerTokens]);

  // --- Card/deck logic (unchanged) ---

  const buyableIds = useMemo(() => getBuyableCardIds(legalActions), [legalActions]);
  const reservableIds = useMemo(() => getReservableCardIds(legalActions), [legalActions]);
  const highlightCards = useMemo(() => [...buyableIds, ...reservableIds], [buyableIds, reservableIds]);

  const reservableDeckTiers = useMemo(() => {
    const tiers = new Set<Tier>();
    for (const a of legalActions) {
      if (a.tag === 'ReserveCard' && a.contents.tag === 'FromTopOfDeck') {
        tiers.add(a.contents.contents);
      }
    }
    return tiers;
  }, [legalActions]);

  const selectedBuyAction = useMemo(() => {
    if (!selectedCardId) return null;
    return legalActions.find(
      (a) =>
        a.tag === 'BuyCard' &&
        (a.contents[0].tag === 'FromDisplay' || a.contents[0].tag === 'FromReserve') &&
        a.contents[0].contents === selectedCardId,
    ) ?? null;
  }, [legalActions, selectedCardId]);

  const selectedReserveAction = useMemo(() => {
    if (!selectedCardId) return null;
    return legalActions.find(
      (a) =>
        a.tag === 'ReserveCard' &&
        a.contents.tag === 'FromDisplay' &&
        a.contents.contents === selectedCardId,
    ) ?? null;
  }, [legalActions, selectedCardId]);

  const selectedDeckReserveAction = useMemo(() => {
    if (!selectedDeckTier) return null;
    return legalActions.find(
      (a) =>
        a.tag === 'ReserveCard' &&
        a.contents.tag === 'FromTopOfDeck' &&
        a.contents.contents === selectedDeckTier,
    ) ?? null;
  }, [legalActions, selectedDeckTier]);

  // --- Gem taking logic ---

  const availableDiffColors = useMemo(() => {
    const colors = new Set<GemColor>();
    for (const a of legalActions) {
      if (a.tag === 'TakeGems' && a.contents.tag === 'TakeDifferent') {
        for (const c of a.contents.contents) {
          colors.add(c);
        }
      }
    }
    return colors;
  }, [legalActions]);

  const availableTwoColors = useMemo(() => {
    const colors = new Set<GemColor>();
    for (const a of legalActions) {
      if (a.tag === 'TakeGems' && a.contents.tag === 'TakeTwoSame') {
        colors.add(a.contents.contents);
      }
    }
    return colors;
  }, [legalActions]);

  const availableGemColors = useMemo(() => {
    const colors = new Set<GemColor>();
    for (const c of availableDiffColors) colors.add(c);
    for (const c of availableTwoColors) colors.add(c);
    return colors;
  }, [availableDiffColors, availableTwoColors]);

  const matchedTakeAction = useMemo((): Action | null => {
    if (selectedBankGems.length === 0) return null;

    // Check take-two-same: all same color, length 2
    if (selectedBankGems.length === 2 && selectedBankGems[0] === selectedBankGems[1]) {
      const color = selectedBankGems[0];
      return legalActions.find(
        (a) => a.tag === 'TakeGems' && a.contents.tag === 'TakeTwoSame' && a.contents.contents === color,
      ) ?? null;
    }

    // Check take-different: all different colors
    const uniqueColors = new Set(selectedBankGems);
    if (uniqueColors.size !== selectedBankGems.length) return null; // duplicates but not a valid take-two

    return legalActions.find((a) => {
      if (a.tag !== 'TakeGems' || a.contents.tag !== 'TakeDifferent') return false;
      const gems = a.contents.contents;
      return gems.length === selectedBankGems.length && selectedBankGems.every((c) => gems.includes(c));
    }) ?? null;
  }, [legalActions, selectedBankGems]);

  // --- Callbacks ---

  const clearSelection = useCallback(() => {
    setSelectedCardId(null);
    setSelectedDeckTier(null);
    setSelectedBankGems([]);
  }, []);

  // --- Gem return confirmation logic ---

  const computeGemsGained = useCallback((action: Action): number => {
    if (action.tag === 'TakeGems') {
      if (action.contents.tag === 'TakeTwoSame') {
        const available = bank[action.contents.contents] ?? 0;
        return Math.min(2, available);
      }
      return action.contents.contents.length;
    }
    if (action.tag === 'ReserveCard') {
      return (bank['Gold'] ?? 0) > 0 ? 1 : 0;
    }
    return 0;
  }, [bank]);

  const excessGems = useMemo(() => {
    if (!pendingAction) return 0;
    return willRequireGemReturn(currentTokenTotal, computeGemsGained(pendingAction));
  }, [pendingAction, currentTokenTotal, computeGemsGained]);

  const confirmPendingAction = useCallback(() => {
    if (pendingAction) {
      send({ tag: 'SubmitAction', contents: pendingAction });
      setPendingAction(null);
      clearSelection();
    }
  }, [pendingAction, send, clearSelection]);

  const cancelPendingAction = useCallback(() => {
    setPendingAction(null);
  }, []);

  /** Send action immediately or set as pending if it will trigger gem return */
  const sendOrConfirm = useCallback((action: Action) => {
    const excess = willRequireGemReturn(currentTokenTotal, computeGemsGained(action));
    if (excess > 0) {
      setPendingAction(action);
    } else {
      send({ tag: 'SubmitAction', contents: action });
      clearSelection();
    }
  }, [currentTokenTotal, computeGemsGained, send, clearSelection]);

  const onBankGemClick = useCallback(
    (color: GemColor) => {
      // Clear card/deck selection when interacting with gems
      setSelectedCardId(null);
      setSelectedDeckTier(null);

      setSelectedBankGems((current) => {
        const countOfColor = current.filter((c) => c === color).length;

        if (countOfColor === 2) {
          // Already double-selected, toggle off entirely
          return [];
        }

        if (countOfColor === 1) {
          const otherGems = current.filter((c) => c !== color);
          if (otherGems.length === 0 && availableTwoColors.has(color)) {
            // Only this color selected, and take-two is legal → select second
            return [color, color];
          }
          // Deselect this color
          return otherGems;
        }

        // countOfColor === 0: adding a new color
        if (current.length === 2 && current[0] === current[1]) {
          // Was a take-two, switching to different mode — start fresh
          return [color];
        }

        if (current.length >= 3) return current;

        return [...current, color];
      });
    },
    [availableTwoColors],
  );

  const handleTakeGems = useCallback(() => {
    if (matchedTakeAction) {
      sendOrConfirm(matchedTakeAction);
    }
  }, [matchedTakeAction, sendOrConfirm]);

  const handleBuy = useCallback(() => {
    if (selectedBuyAction) {
      send({ tag: 'SubmitAction', contents: selectedBuyAction });
      clearSelection();
    }
  }, [selectedBuyAction, send, clearSelection]);

  const handleReserve = useCallback(() => {
    if (selectedReserveAction) {
      sendOrConfirm(selectedReserveAction);
    }
  }, [selectedReserveAction, sendOrConfirm]);

  const handleDeckReserve = useCallback(() => {
    if (selectedDeckReserveAction) {
      sendOrConfirm(selectedDeckReserveAction);
    }
  }, [selectedDeckReserveAction, sendOrConfirm]);

  const onCardClick = useCallback(
    (cardId: CardId) => {
      if (buyableIds.has(cardId) || reservableIds.has(cardId)) {
        setSelectedCardId(cardId);
        setSelectedDeckTier(null);
        setSelectedBankGems([]);
      }
    },
    [buyableIds, reservableIds],
  );

  const onDeckClick = useCallback(
    (tier: Tier) => {
      if (reservableDeckTiers.has(tier)) {
        setSelectedDeckTier(tier);
        setSelectedCardId(null);
        setSelectedBankGems([]);
      }
    },
    [reservableDeckTiers],
  );

  return {
    selectedCardId,
    selectedDeckTier,
    selectedBankGems,
    clearSelection,
    highlightCards,
    reservableDeckTiers,
    onCardClick,
    onDeckClick,
    onBankGemClick,
    selectedBuyAction,
    selectedReserveAction,
    selectedDeckReserveAction,
    matchedTakeAction,
    handleBuy,
    handleReserve,
    handleDeckReserve,
    handleTakeGems,
    availableGemColors,
    pendingAction,
    excessGems,
    confirmPendingAction,
    cancelPendingAction,
  };
}

// --- Helpers moved from removed panel imports ---

function getBuyableCardIds(legalActions: Action[]): Set<CardId> {
  const ids = new Set<CardId>();
  for (const a of legalActions) {
    if (a.tag === 'BuyCard') {
      const src = a.contents[0];
      if (src.tag === 'FromDisplay' || src.tag === 'FromReserve') {
        ids.add(src.contents);
      }
    }
  }
  return ids;
}

function getReservableCardIds(legalActions: Action[]): Set<CardId> {
  const ids = new Set<CardId>();
  for (const a of legalActions) {
    if (a.tag === 'ReserveCard' && a.contents.tag === 'FromDisplay') {
      ids.add(a.contents.contents);
    }
  }
  return ids;
}
