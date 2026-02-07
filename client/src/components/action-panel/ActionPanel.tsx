import { useState, useCallback, useMemo } from 'react';
import type { PublicGameView, ClientMessage, CardId, Tier } from '../../types';
import { useGameStore } from '../../stores/gameStore';
import { TakeGemsPanel } from './TakeGemsPanel';
import { BuyCardPanel, getBuyableCardIds } from './BuyCardPanel';
import { ReserveCardPanel, getReservableCardIds } from './ReserveCardPanel';
import { GemReturnPanel } from './GemReturnPanel';
import { NobleChoicePanel } from './NobleChoicePanel';

interface Props {
  gameView: PublicGameView;
  selfPlayerId: string | null;
  send: (msg: ClientMessage) => void;
  selectedCardId: CardId | null;
  onClearSelection: () => void;
}

export function ActionPanel({ gameView, selfPlayerId, send, selectedCardId, onClearSelection }: Props) {
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
      <div className="text-center text-gray-500 py-4">
        Waiting for {currentPlayer?.ppPlayerName ?? '...'}...
      </div>
    );
  }

  if (legalActions.length === 0) {
    return (
      <div className="text-center text-gray-500 py-4">
        Waiting for server...
      </div>
    );
  }

  return (
    <div className="space-y-4">
      <TakeGemsPanel
        legalActions={legalActions}
        board={gameView.pgvBoard}
        send={send}
      />
      <BuyCardPanel
        legalActions={legalActions}
        gameView={gameView}
        selfPlayerId={selfPlayerId}
        selectedCardId={selectedCardId}
        send={send}
        onClearSelection={onClearSelection}
      />
      <ReserveCardPanel
        legalActions={legalActions}
        selectedCardId={selectedCardId}
        send={send}
        onClearSelection={onClearSelection}
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

  const buyableIds = useMemo(() => getBuyableCardIds(legalActions), [legalActions]);
  const reservableIds = useMemo(() => getReservableCardIds(legalActions), [legalActions]);
  const highlightCards = useMemo(() => [...buyableIds, ...reservableIds], [buyableIds, reservableIds]);

  const onCardClick = useCallback(
    (cardId: CardId) => {
      if (buyableIds.has(cardId) || reservableIds.has(cardId)) {
        setSelectedCardId(cardId);
      }
    },
    [buyableIds, reservableIds],
  );

  const onDeckClick = useCallback(
    (tier: Tier) => {
      const action = legalActions.find(
        (a) =>
          a.tag === 'ReserveCard' &&
          a.contents.tag === 'FromTopOfDeck' &&
          a.contents.contents === tier,
      );
      if (action) {
        send({ tag: 'SubmitAction', contents: action });
      }
    },
    [legalActions, send],
  );

  return {
    selectedCardId,
    setSelectedCardId,
    highlightCards,
    onCardClick,
    onDeckClick,
  };
}
