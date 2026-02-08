import type { ReactNode } from 'react';

interface Props {
  onBuy?: () => void;
  onReserve?: () => void;
  onCancel: () => void;
  paymentBreakdown?: ReactNode;
}

export function CardActionOverlay({ onBuy, onReserve, onCancel, paymentBreakdown }: Props) {
  return (
    <>
      {/* Dark overlay covering the card */}
      <div className="absolute inset-0 z-10 bg-black/75 rounded-lg flex flex-col items-center justify-center gap-2 p-2">
        {onBuy && (
          <button
            type="button"
            onClick={(e) => { e.stopPropagation(); onBuy(); }}
            className="w-full px-3 py-1.5 bg-gradient-to-r from-blue-600 to-blue-700 text-white text-sm rounded-lg font-semibold shadow-md transition-all duration-200 hover:from-blue-700 hover:to-blue-800 hover:shadow-lg"
          >
            Buy
          </button>
        )}
        {paymentBreakdown}
        {onReserve && (
          <button
            type="button"
            onClick={(e) => { e.stopPropagation(); onReserve(); }}
            className="w-full px-3 py-1.5 bg-gradient-to-r from-amber-500 to-yellow-600 text-white text-sm rounded-lg font-semibold shadow-md transition-all duration-200 hover:from-amber-600 hover:to-yellow-700 hover:shadow-lg"
          >
            Reserve
          </button>
        )}
        <button
          type="button"
          onClick={(e) => { e.stopPropagation(); onCancel(); }}
          className="w-full px-3 py-1.5 border border-gray-400/50 text-gray-300 text-sm rounded-lg font-medium transition-all duration-200 hover:bg-gray-600/50"
        >
          Cancel
        </button>
      </div>

    </>
  );
}
