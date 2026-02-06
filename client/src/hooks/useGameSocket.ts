import { useEffect, useRef, useCallback } from 'react';
import { connectGame, type GameSocket } from '../services/websocket';
import { useGameStore } from '../stores/gameStore';
import type { ClientMessage } from '../types';

export function useGameSocket(gameId: string | null, sessionId: string | null) {
  const socketRef = useRef<GameSocket | null>(null);
  const retriesRef = useRef(0);
  const mountedRef = useRef(true);

  const handleMessage = useGameStore((s) => s.handleServerMessage);
  const setConnected = useGameStore((s) => s.setConnected);

  const connect = useCallback(() => {
    if (!gameId || !sessionId || !mountedRef.current) return;

    socketRef.current = connectGame(
      gameId,
      sessionId,
      (msg) => {
        handleMessage(msg);
        // Reset retry counter on successful message
        retriesRef.current = 0;
      },
      () => {
        // onClose — reconnect with exponential backoff
        setConnected(false);
        if (!mountedRef.current) return;
        const delay = Math.min(1000 * Math.pow(2, retriesRef.current), 30000);
        retriesRef.current += 1;
        setTimeout(() => {
          if (mountedRef.current) connect();
        }, delay);
      },
      () => {
        setConnected(false);
      },
      () => {
        // onOpen — only mark connected after WebSocket handshake completes
        if (mountedRef.current) setConnected(true);
      },
    );
  }, [gameId, sessionId, handleMessage, setConnected]);

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
