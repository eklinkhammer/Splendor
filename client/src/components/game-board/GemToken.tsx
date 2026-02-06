import type { TokenType } from '../../types';

const TOKEN_STYLES: Record<TokenType, string> = {
  Diamond: 'bg-white border-gray-300 text-gray-800',
  Sapphire: 'bg-blue-600 border-blue-700 text-white',
  Emerald: 'bg-green-600 border-green-700 text-white',
  Ruby: 'bg-red-600 border-red-700 text-white',
  Onyx: 'bg-gray-800 border-gray-900 text-white',
  Gold: 'bg-yellow-400 border-yellow-500 text-gray-800',
};

interface Props {
  token: TokenType;
  count: number;
  onClick?: () => void;
  selected?: boolean;
  size?: 'sm' | 'md';
}

export function GemToken({ token, count, onClick, selected, size = 'md' }: Props) {
  const sizeClass = size === 'sm' ? 'w-8 h-8 text-xs' : 'w-10 h-10 text-sm';
  return (
    <button
      type="button"
      onClick={onClick}
      disabled={!onClick}
      className={`${sizeClass} rounded-full border-2 font-bold flex items-center justify-center
        ${TOKEN_STYLES[token]}
        ${selected ? 'ring-2 ring-offset-1 ring-blue-400' : ''}
        ${onClick ? 'cursor-pointer hover:scale-110 transition-transform' : 'cursor-default'}`}
      title={`${token}: ${count}`}
    >
      {count}
    </button>
  );
}
