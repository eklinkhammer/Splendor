import type { PublicGameView, PublicPlayer, PlayerId, GemColor } from '../types';
import { ALL_GEM_COLORS } from '../types';

export interface MoveLogEntry {
  turnNumber: number;
  playerId: PlayerId;
  playerName: string;
  description: string;
}

function describeMove(prevPlayer: PublicPlayer, currPlayer: PublicPlayer): string {
  if (currPlayer.ppPurchased.length > prevPlayer.ppPurchased.length) {
    const newCard = currPlayer.ppPurchased[currPlayer.ppPurchased.length - 1];
    if (newCard) {
      let desc = `Bought a ${newCard.cardBonus} card`;
      if (newCard.cardPrestige > 0) {
        desc += ` (${newCard.cardPrestige} VP)`;
      }
      return desc;
    }
  }

  if (currPlayer.ppReservedCount > prevPlayer.ppReservedCount) {
    const prevGold = prevPlayer.ppTokens['Gold'] ?? 0;
    const currGold = currPlayer.ppTokens['Gold'] ?? 0;
    let desc = 'Reserved a card';
    if (currGold > prevGold) {
      desc += ' (+1 Gold)';
    }
    return desc;
  }

  // Took gems
  const gained: string[] = [];
  let tookTwo: GemColor | null = null;
  for (const color of ALL_GEM_COLORS) {
    const prevCount = prevPlayer.ppTokens[color] ?? 0;
    const currCount = currPlayer.ppTokens[color] ?? 0;
    const diff = currCount - prevCount;
    if (diff >= 2) {
      tookTwo = color;
    } else if (diff > 0) {
      gained.push(color);
    }
  }

  if (tookTwo) {
    return `Took 2 ${tookTwo}`;
  }
  if (gained.length > 0) {
    return `Took gems: ${gained.join(', ')}`;
  }
  return 'Took an action';
}

export function computeMoveLogEntry(
  prev: PublicGameView,
  curr: PublicGameView,
): MoveLogEntry | null {
  const prevPlayerIdx = prev.pgvCurrentPlayer;
  const prevPlayer = prev.pgvPlayers[prevPlayerIdx];
  const currPlayer = curr.pgvPlayers[prevPlayerIdx];

  if (!prevPlayer || !currPlayer) return null;

  let description = describeMove(prevPlayer, currPlayer);

  if (currPlayer.ppNobles.length > prevPlayer.ppNobles.length) {
    description += ' — earned a Noble!';
  }

  return {
    turnNumber: prev.pgvTurnNumber,
    playerId: prevPlayer.ppPlayerId,
    playerName: prevPlayer.ppPlayerName,
    description,
  };
}
