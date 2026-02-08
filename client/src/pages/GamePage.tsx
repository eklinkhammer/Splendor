import { useEffect, useMemo, useState } from 'react';
import { useParams, useNavigate, useLocation } from 'react-router-dom';
import { useGameSocket } from '../hooks/useGameSocket';
import { useSessionStore } from '../stores/sessionStore';
import { useGameStore } from '../stores/gameStore';
import { getGame } from '../services/api';
import { toDisplayEntries } from '../types';
import type { GemColor } from '../types';
import { GameBoard } from '../components/game-board/GameBoard';
import { GameStatus } from '../components/game-board/GameStatus';
import { PlayerArea } from '../components/player-area/PlayerArea';
import { useActionCallbacks } from '../components/action-panel/ActionPanel';
import { GemReturnPanel } from '../components/action-panel/GemReturnPanel';
import { NobleChoicePanel } from '../components/action-panel/NobleChoicePanel';
import { CardActionOverlay } from '../components/game-board/CardActionOverlay';
import { GemToken } from '../components/game-board/GemToken';
import { ErrorBanner } from '../components/ErrorBanner';
import { GameOverOverlay } from '../components/game-board/GameOverOverlay';
import { MoveLog } from '../components/game-board/MoveLog';
import { ChatPanel } from '../components/game-board/ChatPanel';

export function GamePage() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const location = useLocation();
  const isSpectator = location.pathname.endsWith('/spectate');
  const sessionId = useSessionStore((s) => s.sessionId);
  const localSessions = useSessionStore((s) => s.localSessions);
  const gameView = useGameStore((s) => s.gameView);
  const gameResult = useGameStore((s) => s.gameResult);
  const selfPlayerId = useGameStore((s) => s.selfPlayerId);
  const gemReturnInfo = useGameStore((s) => s.gemReturnInfo);
  const nobleChoices = useGameStore((s) => s.nobleChoices);
  const connected = useGameStore((s) => s.connected);
  const reset = useGameStore((s) => s.reset);

  // Redirect if no session (skip for spectators)
  useEffect(() => {
    if (!isSpectator && !sessionId) {
      navigate('/');
    }
  }, [isSpectator, sessionId, navigate]);

  // REST fallback: load game state if WebSocket hasn't delivered yet (players only)
  useEffect(() => {
    if (id && sessionId && !gameView && !isSpectator) {
      getGame(id, sessionId).then((view) => {
        useGameStore.getState().handleServerMessage(sessionId, {
          tag: 'GameStateUpdate',
          contents: view,
        });
      }).catch(() => {
        // WebSocket will deliver state
      });
    }
  }, [id, sessionId, gameView, isSpectator]);

  // Build sessions list for WebSocket connections
  const allSessions = useMemo(() => {
    if (isSpectator || !sessionId) return [];
    const primary = [{ sessionId }];
    return [...primary, ...localSessions.map((s) => ({ sessionId: s.sessionId }))];
  }, [isSpectator, sessionId, localSessions]);

  const isHotseat = localSessions.length > 0;
  const { send } = useGameSocket(id ?? null, allSessions, isSpectator);

  const {
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
    handleBuy,
    handleReserve,
    handleDeckReserve,
    matchedTakeAction,
    handleTakeGems,
    availableGemColors,
  } = useActionCallbacks(send);

  // Build selectedGemCounts map from selectedBankGems array
  const selectedGemCounts = useMemo(() => {
    const counts: Partial<Record<GemColor, number>> = {};
    for (const color of selectedBankGems) {
      counts[color] = (counts[color] ?? 0) + 1;
    }
    return counts;
  }, [selectedBankGems]);

  // Build the Take button for the gem bank
  const bankTakeAction = useMemo(() => {
    if (isSpectator || selectedBankGems.length === 0) return undefined;
    return (
      <div className="flex justify-center mt-2">
        <button
          onClick={handleTakeGems}
          disabled={!matchedTakeAction}
          className="px-6 py-1.5 bg-gradient-to-r from-blue-600 to-blue-700 text-white text-sm rounded-xl font-semibold shadow-md transition-all duration-200 hover:from-blue-700 hover:to-blue-800 hover:shadow-lg disabled:opacity-40 disabled:cursor-not-allowed"
        >
          Take
        </button>
      </div>
    );
  }, [isSpectator, selectedBankGems.length, matchedTakeAction, handleTakeGems]);

  // Build the card action overlay for the currently selected card
  const selectedCardOverlay = useMemo(() => {
    if (isSpectator) return null;
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
  }, [isSpectator, selectedCardId, selectedBuyAction, selectedReserveAction, handleBuy, handleReserve, clearSelection, gameView]);

  // Build the deck action overlay for the currently selected deck
  const selectedDeckOverlay = useMemo(() => {
    if (isSpectator || !selectedDeckTier) return null;
    return (
      <CardActionOverlay
        onReserve={handleDeckReserve}
        onCancel={clearSelection}
      />
    );
  }, [isSpectator, selectedDeckTier, handleDeckReserve, clearSelection]);

  // Cleanup on unmount — reset game state and clear stale session IDs
  useEffect(() => {
    return () => {
      reset();
      useSessionStore.getState().setGameId(null);
      useSessionStore.getState().setLobbyId(null);
    };
  }, [reset]);

  const [mobileTab, setMobileTab] = useState<'players' | 'log' | 'chat'>('players');
  const [sidebarTab, setSidebarTab] = useState<'moves' | 'chat'>('moves');
  const chatMessages = useGameStore((s) => s.chatMessages);
  const [chatSeen, setChatSeen] = useState(0);
  const chatUnread = chatMessages.length > chatSeen;

  if (!id || (!isSpectator && !sessionId)) return null;

  if (!gameView) {
    return (
      <div className="min-h-screen bg-gray-900 flex items-center justify-center">
        <p className="text-gray-400">{isSpectator ? 'Connecting as spectator...' : 'Connecting to game...'}</p>
      </div>
    );
  }

  const movesOrChat = (
    <>
      <div className="flex border-b border-gray-700 mb-2">
        <button
          type="button"
          onClick={() => { setSidebarTab('moves'); }}
          className={`flex-1 py-1.5 text-xs font-semibold text-center transition-colors ${
            sidebarTab === 'moves'
              ? 'text-white border-b-2 border-blue-500'
              : 'text-gray-400 hover:text-gray-200'
          }`}
        >
          Moves
        </button>
        <button
          type="button"
          onClick={() => { setSidebarTab('chat'); setChatSeen(chatMessages.length); }}
          className={`flex-1 py-1.5 text-xs font-semibold text-center transition-colors relative ${
            sidebarTab === 'chat'
              ? 'text-white border-b-2 border-blue-500'
              : 'text-gray-400 hover:text-gray-200'
          }`}
        >
          Chat
          {chatUnread && sidebarTab !== 'chat' && (
            <span className="absolute top-1 ml-0.5 w-2 h-2 bg-blue-500 rounded-full" />
          )}
        </button>
      </div>
      {sidebarTab === 'moves' ? <MoveLog /> : <ChatPanel send={send} readOnly={isSpectator} />}
    </>
  );

  const sidebarContent = (
    <>
      <PlayerArea
        players={gameView.pgvPlayers}
        selfPlayerId={isSpectator ? null : selfPlayerId}
        currentPlayerIndex={gameView.pgvCurrentPlayer}
        onReservedCardClick={isSpectator ? undefined : onCardClick}
        selectedCardId={isSpectator ? null : selectedCardId}
        selectedCardOverlay={isSpectator ? null : selectedCardOverlay}
      />
      <div className="mt-3">
        {movesOrChat}
      </div>
    </>
  );

  return (
    <div className="min-h-screen bg-gray-900 p-2 sm:p-4">
      <div className="max-w-6xl mx-auto">
        {isSpectator && (
          <div className="mb-2 px-4 py-2 bg-gradient-to-r from-indigo-500/90 to-purple-500/90 text-white text-sm font-medium rounded-xl shadow-md flex items-center justify-center gap-2">
            Spectating
          </div>
        )}

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

        <div className="mb-2 sm:mb-3">
          <GameStatus gameView={gameView} selfPlayerId={isSpectator ? null : selfPlayerId} isHotseat={isHotseat} />
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-[1fr_280px] gap-4">
          <div>
            <GameBoard
              board={gameView.pgvBoard}
              onCardClick={isSpectator ? undefined : onCardClick}
              onDeckClick={isSpectator ? undefined : onDeckClick}
              highlightCards={isSpectator ? undefined : highlightCards}
              selectedCardId={isSpectator ? null : selectedCardId}
              selectedCardOverlay={isSpectator ? null : selectedCardOverlay}
              selectedDeckTier={isSpectator ? null : selectedDeckTier}
              selectedDeckOverlay={isSpectator ? null : selectedDeckOverlay}
              reservableDeckTiers={isSpectator ? undefined : reservableDeckTiers}
              onBankGemClick={isSpectator ? undefined : onBankGemClick}
              selectedGemCounts={isSpectator ? undefined : selectedGemCounts}
              availableGemColors={isSpectator ? undefined : availableGemColors}
              bankTakeAction={isSpectator ? undefined : bankTakeAction}
            />
            {!isSpectator && nobleChoices && nobleChoices.length > 0 ? (
              <div className="mt-4 bg-gradient-to-b from-gray-800 to-gray-900 rounded-xl p-4 shadow-lg">
                <NobleChoicePanel nobles={nobleChoices} send={send} />
              </div>
            ) : !isSpectator && gemReturnInfo ? (
              <div className="mt-4 bg-gradient-to-b from-gray-800 to-gray-900 rounded-xl p-4 shadow-lg">
                <GemReturnPanel
                  amount={gemReturnInfo.amount}
                  options={gemReturnInfo.options}
                  playerTokens={gameView.pgvPlayers.find((p) => p.ppPlayerId === selfPlayerId)?.ppTokens ?? {}}
                  send={send}
                />
              </div>
            ) : null}
          </div>

          {/* Desktop sidebar */}
          <div className="hidden lg:block">
            {sidebarContent}
          </div>
        </div>

        {/* Mobile tabbed sidebar */}
        <div className="lg:hidden mt-4">
          <div className="flex border-b border-gray-700 mb-3">
            <button
              type="button"
              onClick={() => setMobileTab('players')}
              className={`flex-1 py-2 text-sm font-semibold text-center transition-colors ${
                mobileTab === 'players'
                  ? 'text-white border-b-2 border-blue-500'
                  : 'text-gray-400 hover:text-gray-200'
              }`}
            >
              Players
            </button>
            <button
              type="button"
              onClick={() => setMobileTab('log')}
              className={`flex-1 py-2 text-sm font-semibold text-center transition-colors ${
                mobileTab === 'log'
                  ? 'text-white border-b-2 border-blue-500'
                  : 'text-gray-400 hover:text-gray-200'
              }`}
            >
              Moves
            </button>
            <button
              type="button"
              onClick={() => { setMobileTab('chat'); setChatSeen(chatMessages.length); }}
              className={`flex-1 py-2 text-sm font-semibold text-center transition-colors relative ${
                mobileTab === 'chat'
                  ? 'text-white border-b-2 border-blue-500'
                  : 'text-gray-400 hover:text-gray-200'
              }`}
            >
              Chat
              {chatUnread && mobileTab !== 'chat' && (
                <span className="absolute top-1 ml-0.5 w-2 h-2 bg-blue-500 rounded-full" />
              )}
            </button>
          </div>
          {mobileTab === 'players' ? (
            <PlayerArea
              players={gameView.pgvPlayers}
              selfPlayerId={isSpectator ? null : selfPlayerId}
              currentPlayerIndex={gameView.pgvCurrentPlayer}
              onReservedCardClick={isSpectator ? undefined : onCardClick}
              selectedCardId={isSpectator ? null : selectedCardId}
              selectedCardOverlay={isSpectator ? null : selectedCardOverlay}
            />
          ) : mobileTab === 'log' ? (
            <MoveLog />
          ) : (
            <ChatPanel send={send} readOnly={isSpectator} />
          )}
        </div>
      </div>

      {gameResult && !isSpectator && <GameOverOverlay result={gameResult} />}
    </div>
  );
}
