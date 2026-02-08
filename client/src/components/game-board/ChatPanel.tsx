import { useEffect, useRef, useState } from 'react';
import { useGameStore } from '../../stores/gameStore';
import type { ClientMessage } from '../../types';

interface Props {
  send: (msg: ClientMessage) => void;
  readOnly?: boolean;
}

export function ChatPanel({ send, readOnly }: Props) {
  const chatMessages = useGameStore((s) => s.chatMessages);
  const scrollRef = useRef<HTMLDivElement>(null);
  const [input, setInput] = useState('');

  useEffect(() => {
    if (scrollRef.current) {
      scrollRef.current.scrollTop = scrollRef.current.scrollHeight;
    }
  }, [chatMessages.length]);

  const handleSend = () => {
    const text = input.trim();
    if (!text) return;
    send({ tag: 'SendChat', contents: text });
    setInput('');
  };

  return (
    <div className="bg-gray-800 rounded-xl p-3 flex flex-col">
      <div className="text-xs uppercase tracking-wider text-gray-400 font-semibold mb-2">
        Chat
      </div>
      <div ref={scrollRef} className="max-h-48 overflow-y-auto space-y-1.5 flex-1">
        {chatMessages.length === 0 ? (
          <p className="text-xs text-gray-500">No messages yet.</p>
        ) : (
          chatMessages.map((msg, i) => (
            <div key={i} className="min-w-0">
              <span className="text-xs font-bold text-gray-200">{msg.sender}</span>
              <span className="text-xs text-gray-400 ml-1">{msg.message}</span>
            </div>
          ))
        )}
      </div>
      {!readOnly && (
        <div className="mt-2 flex gap-1.5">
          <input
            type="text"
            value={input}
            onChange={(e) => setInput(e.target.value)}
            onKeyDown={(e) => e.key === 'Enter' && handleSend()}
            maxLength={200}
            placeholder="Type a message..."
            className="flex-1 bg-gray-700 text-gray-200 text-xs rounded-lg px-2.5 py-1.5 outline-none focus:ring-1 focus:ring-blue-500 placeholder-gray-500"
          />
          <button
            type="button"
            onClick={handleSend}
            disabled={!input.trim()}
            className="px-3 py-1.5 bg-blue-600 text-white text-xs rounded-lg font-semibold hover:bg-blue-700 disabled:opacity-40 disabled:cursor-not-allowed"
          >
            Send
          </button>
        </div>
      )}
    </div>
  );
}
