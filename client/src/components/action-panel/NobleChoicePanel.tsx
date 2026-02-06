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
    <div className="p-3 bg-purple-50 border border-purple-200 rounded-lg space-y-2">
      <h3 className="text-sm font-semibold text-purple-800">Choose a Noble</h3>
      <p className="text-xs text-purple-600">
        Multiple nobles are available. Choose one to visit you.
      </p>
      <NobleRow nobles={nobles} onNobleClick={handleNobleClick} />
    </div>
  );
}
