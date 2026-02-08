import { useEffect, useMemo } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { useGameSocket } from '../hooks/useGameSocket';
import { useSessionStore } from '../stores/sessionStore';
import { useGameStore } from '../stores/gameStore';
import { getGame } from '../services/api';
import { toDisplayEntries } from '../types';
import { GameBoard } from '../components/game-board/GameBoard';
import { GameStatus } from '../components/game-board/GameStatus';
import { PlayerArea } from '../components/player-area/PlayerArea';
import { ActionPanel, useActionCallbacks } from '../components/action-panel/ActionPanel';
import { CardActionOverlay } from '../components/game-board/CardActionOverlay';
import { GemToken } from '../components/game-board/GemToken';
import { ErrorBanner } from '../components/ErrorBanner';
import { GameOverOverlay } from '../components/game-board/GameOverOverlay';
import { MoveLog } from '../components/game-board/MoveLog';

export function GamePage() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const sessionId = useSessionStore((s) => s.sessionId);
  const gameView = useGameStore((s) => s.gameView);
  const gameResult = useGameStore((s) => s.gameResult);
  const selfPlayerId = useGameStore((s) => s.selfPlayerId);
  const connected = useGameStore((s) => s.connected);
  const reset = useGameStore((s) => s.reset);
  const setGameId = useSessionStore((s) => s.setGameId);
  const setLobbyId = useSessionStore((s) => s.setLobbyId);

  // Redirect if no session
  useEffect(() => {
    if (!sessionId) {
      navigate('/');
    }
  }, [sessionId, navigate]);

  // REST fallback: load game state if WebSocket hasn't delivered yet
  useEffect(() => {
    if (id && sessionId && !gameView) {
      getGame(id, sessionId).then((view) => {
        useGameStore.getState().handleServerMessage({
          tag: 'GameStateUpdate',
          contents: view,
        });
      }).catch(() => {
        // WebSocket will deliver state
      });
    }
  }, [id, sessionId, gameView]);

  const { send } = useGameSocket(id ?? null, sessionId);

  const {
    selectedCardId,
    selectedDeckTier,
    clearSelection,
    highlightCards,
    reservableDeckTiers,
    onCardClick,
    onDeckClick,
    selectedBuyAction,
    selectedReserveAction,
    handleBuy,
    handleReserve,
    handleDeckReserve,
  } = useActionCallbacks(send);

  // Build the card action overlay for the currently selected card
  const selectedCardOverlay = useMemo(() => {
    if (!selectedCardId || (!selectedBuyAction && !selectedReserveAction)) return null;

    // Build payment breakdown for buy actions
    let paymentBreakdown: React.ReactNode = null;
    if (selectedBuyAction && selectedBuyAction.tag === 'BuyCard' && gameView) {
      const payment = selectedBuyAction.contents[1];
      const paymentEntries = toDisplayEntries(payment);
      if (paymentEntries.length > 0) {
        paymentBreakdown = (
          <div className="w-full space-y-0.5">
            <div className="flex items-center justify-center gap-1">
              <div className="flex gap-0.5">
                {paymentEntries.map(([token, count]) => (
                  <GemToken key={token} token={token} count={count} size="sm" />
                ))}
              </div>
            </div>
          </div>
        );
      }
    }

    return (
      <CardActionOverlay
        onBuy={selectedBuyAction ? handleBuy : undefined}
        onReserve={selectedReserveAction ? handleReserve : undefined}
        onCancel={clearSelection}
        paymentBreakdown={paymentBreakdown}
      />
    );
  }, [selectedCardId, selectedBuyAction, selectedReserveAction, handleBuy, handleReserve, clearSelection, gameView, selfPlayerId]);

  // Build the deck action overlay for the currently selected deck
  const selectedDeckOverlay = useMemo(() => {
    if (!selectedDeckTier) return null;
    return (
      <CardActionOverlay
        onReserve={handleDeckReserve}
        onCancel={clearSelection}
      />
    );
  }, [selectedDeckTier, handleDeckReserve, clearSelection]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      reset();
      setGameId(null);
      setLobbyId(null);
    };
  }, [reset, setGameId, setLobbyId]);

  if (!id || !sessionId) return null;

  if (!gameView) {
    return (
      <div className="min-h-screen bg-gray-900 flex items-center justify-center">
        <p className="text-gray-400">Connecting to game...</p>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-900 p-4">
      <div className="max-w-6xl mx-auto">
        {!connected && (
          <div className="mb-2 px-4 py-2 bg-gradient-to-r from-amber-500/90 to-yellow-500/90 text-white text-sm font-medium rounded-xl shadow-md flex items-center justify-center gap-2">
            <span className="relative flex h-2 w-2">
              <span className="animate-ping absolute inline-flex h-full w-full rounded-full bg-white opacity-75" />
              <span className="relative inline-flex h-2 w-2 rounded-full bg-white" />
            </span>
            Reconnecting...
          </div>
        )}

        <ErrorBanner />

        <div className="mb-3">
          <GameStatus gameView={gameView} selfPlayerId={selfPlayerId} />
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-[1fr_280px] gap-4">
          <div>
            <GameBoard
              board={gameView.pgvBoard}
              onCardClick={onCardClick}
              onDeckClick={onDeckClick}
              highlightCards={highlightCards}
              selectedCardId={selectedCardId}
              selectedCardOverlay={selectedCardOverlay}
              selectedDeckTier={selectedDeckTier}
              selectedDeckOverlay={selectedDeckOverlay}
              reservableDeckTiers={reservableDeckTiers}
            />
            <div className="mt-4 bg-gradient-to-b from-gray-800 to-gray-900 rounded-xl p-4 shadow-lg">
              <ActionPanel
                gameView={gameView}
                selfPlayerId={selfPlayerId}
                send={send}
              />
            </div>
          </div>

          <div>
            <PlayerArea
              players={gameView.pgvPlayers}
              selfPlayerId={selfPlayerId}
              currentPlayerIndex={gameView.pgvCurrentPlayer}
              onReservedCardClick={onCardClick}
              selectedCardId={selectedCardId}
              selectedCardOverlay={selectedCardOverlay}
            />
            <div className="mt-3">
              <MoveLog />
            </div>
          </div>
        </div>
      </div>

      {gameResult && <GameOverOverlay result={gameResult} />}
    </div>
  );
}
