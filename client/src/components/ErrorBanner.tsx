import { useEffect } from 'react';
import { useGameStore } from '../stores/gameStore';

export function ErrorBanner() {
  const error = useGameStore((s) => s.error);
  const clearError = useGameStore((s) => s.clearError);

  useEffect(() => {
    if (error) {
      const timer = setTimeout(clearError, 5000);
      return () => clearTimeout(timer);
    }
  }, [error, clearError]);

  if (!error) return null;

  return (
    <div className="mb-2 px-4 py-2 bg-gradient-to-r from-red-600/90 to-red-700/90 text-white rounded-xl shadow-md flex items-center justify-between">
      <div className="flex items-center gap-2">
        <span className="flex items-center justify-center w-5 h-5 bg-white/20 rounded-full text-xs font-bold">!</span>
        <span className="text-sm">{error}</span>
      </div>
      <button onClick={clearError} className="text-white/80 hover:text-white font-bold ml-2 transition-colors duration-200">
        &times;
      </button>
    </div>
  );
}
