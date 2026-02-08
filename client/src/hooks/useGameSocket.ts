import { useEffect, useRef, useCallback } from 'react';
import { connectGame, connectSpectator, type GameSocket } from '../services/websocket';
import { useGameStore } from '../stores/gameStore';
import type { ClientMessage } from '../types';

interface SessionEntry {
  sessionId: string;
}

export function useGameSocket(
  gameId: string | null,
  sessions: SessionEntry[],
  spectator?: boolean,
) {
  const socketsRef = useRef<Map<string, GameSocket>>(new Map());
  const retriesRef = useRef<Map<string, number>>(new Map());
  const openRef = useRef<Set<string>>(new Set());
  const mountedRef = useRef(true);

  const handleMessage = useGameStore((s) => s.handleServerMessage);
  const setConnected = useGameStore((s) => s.setConnected);

  const totalExpected = spectator ? 1 : sessions.length;

  const updateConnected = useCallback(() => {
    setConnected(openRef.current.size >= totalExpected && totalExpected > 0);
  }, [setConnected, totalExpected]);

  const connectOne = useCallback((key: string, isSpectator: boolean, sessionId?: string) => {
    if (!gameId || !mountedRef.current) return;

    const onMessage = (msg: Parameters<typeof handleMessage>[1]) => {
      handleMessage(key, msg);
      retriesRef.current.set(key, 0);
    };
    const onClose = () => {
      openRef.current.delete(key);
      updateConnected();
      if (!mountedRef.current) return;
      const retries = retriesRef.current.get(key) ?? 0;
      const delay = Math.min(1000 * Math.pow(2, retries), 30000);
      retriesRef.current.set(key, retries + 1);
      setTimeout(() => {
        if (mountedRef.current) connectOne(key, isSpectator, sessionId);
      }, delay);
    };
    const onError = () => {
      openRef.current.delete(key);
      updateConnected();
    };
    const onOpen = () => {
      if (mountedRef.current) {
        openRef.current.add(key);
        updateConnected();
      }
    };

    let socket: GameSocket;
    if (isSpectator) {
      socket = connectSpectator(gameId, onMessage, onClose, onError, onOpen);
    } else {
      socket = connectGame(gameId, sessionId!, onMessage, onClose, onError, onOpen);
    }
    socketsRef.current.set(key, socket);
  }, [gameId, handleMessage, updateConnected]);

  useEffect(() => {
    mountedRef.current = true;
    retriesRef.current.clear();
    openRef.current.clear();

    if (spectator) {
      connectOne('spectator', true);
    } else {
      for (const { sessionId } of sessions) {
        connectOne(sessionId, false, sessionId);
      }
    }

    return () => {
      mountedRef.current = false;
      for (const socket of socketsRef.current.values()) {
        socket.close();
      }
      socketsRef.current.clear();
      openRef.current.clear();
      setConnected(false);
    };
    // Re-connect when gameId or session list changes
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [gameId, spectator, sessions.map((s) => s.sessionId).join(',')]);

  const send = useCallback((msg: ClientMessage) => {
    // In hotseat mode, send through the active session's socket
    const activeSessionId = useGameStore.getState().activeSessionId;
    if (activeSessionId) {
      socketsRef.current.get(activeSessionId)?.send(msg);
    } else {
      // Fallback: send through the first (or only) socket
      const first = socketsRef.current.values().next().value;
      first?.send(msg);
    }
  }, []);

  const connected = useGameStore((s) => s.connected);

  return { send, connected };
}
