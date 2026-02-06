// Types mirroring Haskell Aeson Generic TaggedObject encoding

// --- Gem types ---

export type GemColor = 'Diamond' | 'Sapphire' | 'Emerald' | 'Ruby' | 'Onyx';

export const ALL_GEM_COLORS: GemColor[] = ['Diamond', 'Sapphire', 'Emerald', 'Ruby', 'Onyx'];

export type TokenType = GemColor | 'Gold';

export const ALL_TOKEN_TYPES: TokenType[] = [...ALL_GEM_COLORS, 'Gold'];

// After Step 0 fix: GemCollection serializes as {"Diamond":3,"Gold":1}
export type GemCollection = Partial<Record<TokenType, number>>;

// --- Card types ---

export type Tier = 'Tier1' | 'Tier2' | 'Tier3';

export type CardId = string;

export interface Card {
  cardId: CardId;
  cardTier: Tier;
  cardCost: GemCollection;
  cardBonus: GemColor;
  cardPrestige: number;
}

// --- Noble types ---

export type NobleId = string;

export interface Noble {
  nobleId: NobleId;
  nobleRequirement: Partial<Record<GemColor, number>>;
  noblePrestige: number;
}

// --- Board types ---

export interface PublicTierRow {
  publicDeckCount: number;
  publicDisplay: Card[];
}

export interface PublicBoard {
  publicTier1: PublicTierRow;
  publicTier2: PublicTierRow;
  publicTier3: PublicTierRow;
  publicNobles: Noble[];
  publicBank: GemCollection;
}

// --- Player types ---

export type PlayerId = string;

export interface PublicPlayer {
  ppPlayerId: PlayerId;
  ppPlayerName: string;
  ppTokens: GemCollection;
  ppPurchased: Card[];
  ppReservedCount: number;
  ppReserved: Card[] | null; // Just for self, Nothing for opponents
  ppNobles: Noble[];
  ppPrestige: number;
}

// --- Game state types ---

export interface GameResult {
  winnerId: PlayerId;
  winnerName: string;
  finalPrestige: number;
}

// Aeson TaggedObject: nullary constructors have just "tag", others have "tag"+"contents"
export type GamePhase =
  | { tag: 'InProgress' }
  | { tag: 'FinalRound' }
  | { tag: 'Finished'; contents: GameResult };

export type TurnPhase =
  | { tag: 'AwaitingAction' }
  | { tag: 'MustReturnGems'; contents: number };

export interface PublicGameView {
  pgvGameId: string;
  pgvBoard: PublicBoard;
  pgvPlayers: PublicPlayer[];
  pgvCurrentPlayer: number;
  pgvTurnNumber: number;
  pgvPhase: GamePhase;
  pgvTurnPhase: TurnPhase;
}

// --- Action types ---

export type GemTake =
  | { tag: 'TakeDifferent'; contents: GemColor[] }
  | { tag: 'TakeTwoSame'; contents: GemColor };

export type CardSource =
  | { tag: 'FromDisplay'; contents: CardId }
  | { tag: 'FromReserve'; contents: CardId }
  | { tag: 'FromTopOfDeck'; contents: Tier };

// Multi-field constructors serialize contents as an array
export type Action =
  | { tag: 'TakeGems'; contents: GemTake }
  | { tag: 'BuyCard'; contents: [CardSource, GemCollection] }
  | { tag: 'ReserveCard'; contents: CardSource };

// --- Lobby types ---

export type SessionId = string;
export type LobbyId = string;
export type GameId = string;

export interface LobbySlot {
  lsSessionId: SessionId;
  lsPlayerName: string;
  lsIsAI: boolean;
}

export type LobbyStatus =
  | { tag: 'Waiting' }
  | { tag: 'Starting' }
  | { tag: 'Started'; contents: GameId }
  | { tag: 'Closed' };

export interface Lobby {
  lobbyId: LobbyId;
  lobbyName: string;
  lobbySlots: LobbySlot[];
  lobbyMaxPlayers: number;
  lobbyMinPlayers: number;
  lobbyStatus: LobbyStatus;
  lobbyCreatedAt: string;
}

// --- Request/Response types ---

export interface CreateLobbyRequest {
  clrPlayerName: string;
  clrLobbyName: string;
}

export interface CreateLobbyResponse {
  clrLobbyId: LobbyId;
  clrSessionId: SessionId;
}

export interface JoinLobbyRequest {
  jlrPlayerName: string;
}

export interface JoinLobbyResponse {
  jlrSessionId: SessionId;
}

export interface StartGameResponse {
  sgrGameId: GameId;
}

// --- WebSocket messages ---

export type ClientMessage =
  | { tag: 'SubmitAction'; contents: Action }
  | { tag: 'ReturnGems'; contents: GemCollection }
  | { tag: 'ChooseNoble'; contents: NobleId }
  | { tag: 'Ping' };

export type ServerMessage =
  | { tag: 'GameStateUpdate'; contents: PublicGameView }
  | { tag: 'ActionRequired'; contents: Action[] }
  | { tag: 'GemReturnNeeded'; contents: [number, GemCollection[]] }
  | { tag: 'NobleChoiceRequired'; contents: Noble[] }
  | { tag: 'GameOverMsg'; contents: GameResult }
  | { tag: 'ErrorMsg'; contents: string }
  | { tag: 'Pong' };
