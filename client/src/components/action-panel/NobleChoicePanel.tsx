import type { Noble, ClientMessage } from '../../types';
import { NobleRow } from '../game-board/NobleRow';

interface Props {
  nobles: Noble[];
  send: (msg: ClientMessage) => void;
}

export function NobleChoicePanel({ nobles, send }: Props) {
  const handleNobleClick = (nobleId: string) => {
    send({ tag: 'ChooseNoble', contents: nobleId });
  };

  return (
    <div className="bg-gradient-to-b from-purple-900/60 to-purple-950/60 border border-purple-500/40 rounded-xl p-4 shadow-lg space-y-3">
      <div className="flex items-center gap-2">
        <span className="text-base">&#x1F451;</span>
        <h3 className="text-sm font-bold text-purple-300">Choose a Noble</h3>
        <span className="px-2 py-0.5 text-[10px] font-semibold uppercase tracking-wider bg-purple-500/20 text-purple-300 rounded-full border border-purple-500/30">Required</span>
      </div>
      <p className="text-xs text-purple-200/80">
        Multiple nobles are available. Choose one to visit you.
      </p>
      <NobleRow nobles={nobles} onNobleClick={handleNobleClick} />
    </div>
  );
}
