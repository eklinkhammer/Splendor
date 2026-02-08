import { useEffect, useRef, useCallback } from 'react';
import { connectGame, connectSpectator, type GameSocket } from '../services/websocket';
import { useGameStore } from '../stores/gameStore';
import type { ClientMessage } from '../types';

export function useGameSocket(
  gameId: string | null,
  sessionId: string | null,
  spectator?: boolean,
) {
  const socketRef = useRef<GameSocket | null>(null);
  const retriesRef = useRef(0);
  const mountedRef = useRef(true);

  const handleMessage = useGameStore((s) => s.handleServerMessage);
  const setConnected = useGameStore((s) => s.setConnected);

  const connect = useCallback(() => {
    if (!gameId || !mountedRef.current) return;
    if (!spectator && !sessionId) return;

    const onMessage = (msg: Parameters<typeof handleMessage>[0]) => {
      handleMessage(msg);
      retriesRef.current = 0;
    };
    const onClose = () => {
      setConnected(false);
      if (!mountedRef.current) return;
      const delay = Math.min(1000 * Math.pow(2, retriesRef.current), 30000);
      retriesRef.current += 1;
      setTimeout(() => {
        if (mountedRef.current) connect();
      }, delay);
    };
    const onError = () => {
      setConnected(false);
    };
    const onOpen = () => {
      if (mountedRef.current) setConnected(true);
    };

    if (spectator) {
      socketRef.current = connectSpectator(gameId, onMessage, onClose, onError, onOpen);
    } else {
      socketRef.current = connectGame(gameId, sessionId!, onMessage, onClose, onError, onOpen);
    }
  }, [gameId, sessionId, spectator, handleMessage, setConnected]);

  useEffect(() => {
    mountedRef.current = true;
    retriesRef.current = 0;
    connect();

    return () => {
      mountedRef.current = false;
      socketRef.current?.close();
      socketRef.current = null;
    };
  }, [connect]);

  const send = useCallback((msg: ClientMessage) => {
    socketRef.current?.send(msg);
  }, []);

  const connected = useGameStore((s) => s.connected);

  return { send, connected };
}
