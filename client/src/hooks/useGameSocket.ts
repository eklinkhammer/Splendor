import { useEffect, useRef, useCallback } from 'react';
import { connectGame, connectSpectator, type GameSocket } from '../services/websocket';
import { useGameStore } from '../stores/gameStore';
import type { ClientMessage, ServerMessage } from '../types';

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

  const connectOne = useCallback((isSpectator: boolean, sid?: string) => {
    if (!gameId || !mountedRef.current) return;

    const key = sid ?? 'spectator';
    const onMessage = (msg: ServerMessage) => {
      handleMessage(key, msg);
      retriesRef.current = 0;
    };
    const onClose = () => {
      setConnected(false);
      if (!mountedRef.current) return;
      const retries = retriesRef.current;
      const delay = Math.min(1000 * Math.pow(2, retries), 30000);
      retriesRef.current = retries + 1;
      setTimeout(() => {
        if (mountedRef.current) connectOne(isSpectator, sid);
      }, delay);
    };
    const onError = () => {
      setConnected(false);
    };
    const onOpen = () => {
      if (mountedRef.current) {
        setConnected(true);
      }
    };

    let socket: GameSocket;
    if (isSpectator) {
      socket = connectSpectator(gameId, onMessage, onClose, onError, onOpen);
    } else {
      socket = connectGame(gameId, sid!, onMessage, onClose, onError, onOpen);
    }
    socketRef.current = socket;
  }, [gameId, handleMessage, setConnected]);

  useEffect(() => {
    mountedRef.current = true;
    retriesRef.current = 0;

    if (spectator) {
      connectOne(true);
    } else if (sessionId) {
      connectOne(false, sessionId);
    }

    return () => {
      mountedRef.current = false;
      socketRef.current?.close();
      socketRef.current = null;
      setConnected(false);
    };
  }, [gameId, spectator, sessionId, connectOne, setConnected]);

  const send = useCallback((msg: ClientMessage) => {
    socketRef.current?.send(msg);
  }, []);

  const connected = useGameStore((s) => s.connected);

  return { send, connected };
}
