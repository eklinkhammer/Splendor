import type { TokenType } from '../../types';

const TOKEN_STYLES: Record<TokenType, string> = {
  Diamond: 'bg-gradient-to-br from-white to-gray-200 border-gray-300 text-gray-800',
  Sapphire: 'bg-gradient-to-br from-blue-500 to-blue-700 border-blue-800 text-white',
  Emerald: 'bg-gradient-to-br from-green-500 to-green-700 border-green-800 text-white',
  Ruby: 'bg-gradient-to-br from-red-500 to-red-700 border-red-800 text-white',
  Onyx: 'bg-gradient-to-br from-gray-700 to-gray-900 border-gray-950 text-white',
  Gold: 'bg-gradient-to-br from-yellow-300 to-yellow-500 border-yellow-600 text-gray-800',
};

interface Props {
  token: TokenType;
  count: number;
  onClick?: () => void;
  selectedCount?: number;
  size?: 'sm' | 'md';
}

export function GemToken({ token, count, onClick, selectedCount, size = 'md' }: Props) {
  const sizeClass = size === 'sm'
    ? 'w-[var(--gem-size-sm)] h-[var(--gem-size-sm)] text-[10px] sm:text-xs'
    : 'w-[var(--gem-size-md)] h-[var(--gem-size-md)] text-sm sm:text-base';
  const effectiveCount = selectedCount ?? 0;
  const selectionClass =
    effectiveCount >= 2
      ? 'ring-2 ring-amber-400 ring-offset-2 ring-offset-gray-900 outline outline-2 outline-offset-[6px] outline-amber-400'
      : effectiveCount === 1
        ? 'ring-2 ring-amber-400 ring-offset-2 ring-offset-gray-900'
        : 'ring-1 ring-inset ring-white/30';
  const className = `${sizeClass} rounded-full border-2 font-bold flex items-center justify-center
        shadow-md relative
        ${TOKEN_STYLES[token]}
        ${selectionClass}
        ${onClick ? 'cursor-pointer hover:scale-110 hover:shadow-lg transition-all duration-150' : 'cursor-default'}`;
  const Tag = onClick ? 'button' : 'div';
  return (
    <Tag
      {...(onClick ? { type: 'button' as const, onClick } : {})}
      className={className}
      title={`${token}: ${count}`}
    >
      <span className="drop-shadow-sm">{count}</span>
    </Tag>
  );
}
