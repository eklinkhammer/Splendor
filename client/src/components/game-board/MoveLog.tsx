import { useEffect, useRef } from 'react';
import { useGameStore } from '../../stores/gameStore';

export function MoveLog() {
  const moveLog = useGameStore((s) => s.moveLog);
  const scrollRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (scrollRef.current) {
      scrollRef.current.scrollTop = scrollRef.current.scrollHeight;
    }
  }, [moveLog.length]);

  return (
    <div className="bg-gray-800 rounded-xl p-3">
      <div className="text-xs uppercase tracking-wider text-gray-400 font-semibold mb-2">
        Move History
      </div>
      <div ref={scrollRef} className="max-h-48 overflow-y-auto space-y-1.5">
        {moveLog.length === 0 ? (
          <p className="text-xs text-gray-500">Game in progress...</p>
        ) : (
          moveLog.map((entry, i) => (
            <div key={i} className="flex items-start gap-2">
              <span className="shrink-0 bg-gray-600 text-gray-300 text-[10px] font-bold w-5 h-5 rounded-full flex items-center justify-center">
                {entry.turnNumber}
              </span>
              <div className="min-w-0">
                <span className="text-xs font-bold text-gray-200">{entry.playerName}</span>
                <span className="text-xs text-gray-400 ml-1">{entry.description}</span>
              </div>
            </div>
          ))
        )}
      </div>
    </div>
  );
}
