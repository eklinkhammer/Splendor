import type { ServerMessage, ClientMessage } from '../types';

export interface GameSocket {
  send: (msg: ClientMessage) => void;
  close: () => void;
}

export function connectGame(
  gameId: string,
  sessionId: string,
  onMessage: (msg: ServerMessage) => void,
  onClose: () => void,
  onError: (err: Event) => void,
  onOpen?: () => void,
): GameSocket {
  const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
  const url = `${protocol}//${location.host}/api/v1/games/${gameId}/ws?session=${encodeURIComponent(sessionId)}`;
  const ws = new WebSocket(url);

  let pingInterval: ReturnType<typeof setInterval> | null = null;

  ws.onopen = () => {
    pingInterval = setInterval(() => {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({ tag: 'Ping' }));
      }
    }, 25000);
    onOpen?.();
  };

  ws.onmessage = (event) => {
    try {
      const msg = JSON.parse(event.data as string) as ServerMessage;
      onMessage(msg);
    } catch {
      console.error('Failed to parse server message:', event.data);
    }
  };

  ws.onclose = () => {
    if (pingInterval) clearInterval(pingInterval);
    onClose();
  };

  ws.onerror = (err) => {
    onError(err);
  };

  return {
    send: (msg: ClientMessage) => {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify(msg));
      }
    },
    close: () => {
      if (pingInterval) clearInterval(pingInterval);
      ws.close();
    },
  };
}
