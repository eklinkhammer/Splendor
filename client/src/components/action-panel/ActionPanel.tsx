import { useState, useCallback, useMemo } from 'react';
import type { PublicGameView, ClientMessage, CardId, Tier } from '../../types';
import { useGameStore } from '../../stores/gameStore';
import { TakeGemsPanel } from './TakeGemsPanel';
import { getBuyableCardIds } from './BuyCardPanel';
import { getReservableCardIds } from './ReserveCardPanel';
import { GemReturnPanel } from './GemReturnPanel';
import { NobleChoicePanel } from './NobleChoicePanel';

interface Props {
  gameView: PublicGameView;
  selfPlayerId: string | null;
  send: (msg: ClientMessage) => void;
}

export function ActionPanel({ gameView, selfPlayerId, send }: Props) {
  const legalActions = useGameStore((s) => s.legalActions);
  const gemReturnInfo = useGameStore((s) => s.gemReturnInfo);
  const nobleChoices = useGameStore((s) => s.nobleChoices);

  const currentPlayer = gameView.pgvPlayers[gameView.pgvCurrentPlayer];
  const isMyTurn = currentPlayer?.ppPlayerId === selfPlayerId;

  // Noble choice takes priority
  if (nobleChoices && nobleChoices.length > 0) {
    return <NobleChoicePanel nobles={nobleChoices} send={send} />;
  }

  // Gem return takes priority over regular actions
  if (gemReturnInfo) {
    const self = gameView.pgvPlayers.find((p) => p.ppPlayerId === selfPlayerId);
    return (
      <GemReturnPanel
        amount={gemReturnInfo.amount}
        options={gemReturnInfo.options}
        playerTokens={self?.ppTokens ?? {}}
        send={send}
      />
    );
  }

  if (!isMyTurn) {
    return (
      <div className="text-center py-4 space-y-1">
        <div className="flex items-center justify-center gap-2 text-gray-300">
          <span className="relative flex h-2 w-2">
            <span className="animate-ping absolute inline-flex h-full w-full rounded-full bg-gray-400 opacity-75" />
            <span className="relative inline-flex h-2 w-2 rounded-full bg-gray-400" />
          </span>
          Waiting for {currentPlayer?.ppPlayerName ?? '...'}
        </div>
        <p className="text-xs text-gray-500">Their turn is in progress</p>
      </div>
    );
  }

  if (legalActions.length === 0) {
    return (
      <div className="text-center py-4">
        <div className="flex items-center justify-center gap-2 text-gray-300">
          <span className="relative flex h-2 w-2">
            <span className="animate-ping absolute inline-flex h-full w-full rounded-full bg-blue-400 opacity-75" />
            <span className="relative inline-flex h-2 w-2 rounded-full bg-blue-400" />
          </span>
          Loading actions...
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-3">
      <TakeGemsPanel
        legalActions={legalActions}
        board={gameView.pgvBoard}
        send={send}
      />
    </div>
  );
}

/** Hook for GamePage to get interactive callbacks for the board */
export function useActionCallbacks(
  send: (msg: ClientMessage) => void,
) {
  const legalActions = useGameStore((s) => s.legalActions);
  const [selectedCardId, setSelectedCardId] = useState<CardId | null>(null);
  const [selectedDeckTier, setSelectedDeckTier] = useState<Tier | null>(null);

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

  // Find the buy/reserve actions for the currently selected card
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

  const clearSelection = useCallback(() => {
    setSelectedCardId(null);
    setSelectedDeckTier(null);
  }, []);

  const handleBuy = useCallback(() => {
    if (selectedBuyAction) {
      send({ tag: 'SubmitAction', contents: selectedBuyAction });
      clearSelection();
    }
  }, [selectedBuyAction, send, clearSelection]);

  const handleReserve = useCallback(() => {
    if (selectedReserveAction) {
      send({ tag: 'SubmitAction', contents: selectedReserveAction });
      clearSelection();
    }
  }, [selectedReserveAction, send, clearSelection]);

  const handleDeckReserve = useCallback(() => {
    if (selectedDeckReserveAction) {
      send({ tag: 'SubmitAction', contents: selectedDeckReserveAction });
      clearSelection();
    }
  }, [selectedDeckReserveAction, send, clearSelection]);

  const onCardClick = useCallback(
    (cardId: CardId) => {
      if (buyableIds.has(cardId) || reservableIds.has(cardId)) {
        setSelectedCardId(cardId);
        setSelectedDeckTier(null);
      }
    },
    [buyableIds, reservableIds],
  );

  const onDeckClick = useCallback(
    (tier: Tier) => {
      if (reservableDeckTiers.has(tier)) {
        setSelectedDeckTier(tier);
        setSelectedCardId(null);
      }
    },
    [reservableDeckTiers],
  );

  return {
    selectedCardId,
    selectedDeckTier,
    clearSelection,
    highlightCards,
    reservableDeckTiers,
    onCardClick,
    onDeckClick,
    selectedBuyAction,
    selectedReserveAction,
    selectedDeckReserveAction,
    handleBuy,
    handleReserve,
    handleDeckReserve,
  };
}
