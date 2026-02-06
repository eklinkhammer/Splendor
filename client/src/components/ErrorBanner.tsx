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
    <div className="mb-2 px-4 py-2 bg-red-100 text-red-800 rounded flex items-center justify-between">
      <span className="text-sm">{error}</span>
      <button onClick={clearError} className="text-red-600 hover:text-red-800 font-bold ml-2">
        &times;
      </button>
    </div>
  );
}
