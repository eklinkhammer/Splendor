module Splendor.Core.Types.Gem
  ( GemColor(..)
  , allGemColors
  , TokenType(..)
  , allTokenTypes
  , GemCollection(..)
  , emptyGems
  , singleGem
  , addGems
  , subtractGems
  , gemCount
  , totalGems
  , hasEnoughGems
  , nonNegative
  , gemColors
  ) where

import Data.Aeson (FromJSON, FromJSONKey(..), ToJSON, ToJSONKey(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as E
import Data.Aeson.Key qualified as Key
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data GemColor = Diamond | Sapphire | Emerald | Ruby | Onyx
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

allGemColors :: [GemColor]
allGemColors = [minBound .. maxBound]

data TokenType = GemToken GemColor | GoldToken
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Custom ToJSONKey/FromJSONKey so Map keys render as "Diamond", "Gold" etc.
tokenTypeToText :: TokenType -> Text
tokenTypeToText (GemToken c) = T.pack (show c)
tokenTypeToText GoldToken    = "Gold"

tokenTypeFromText :: Text -> Maybe TokenType
tokenTypeFromText "Gold" = Just GoldToken
tokenTypeFromText t = case reads (T.unpack t) of
  [(c, "")] -> Just (GemToken c)
  _         -> Nothing

instance ToJSONKey TokenType where
  toJSONKey = Aeson.ToJSONKeyText
    (Key.fromText . tokenTypeToText)
    (E.text . tokenTypeToText)

instance FromJSONKey TokenType where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ \t ->
    case tokenTypeFromText t of
      Just tt -> pure tt
      Nothing -> fail $ "Invalid TokenType key: " <> T.unpack t

allTokenTypes :: [TokenType]
allTokenTypes = map GemToken allGemColors ++ [GoldToken]

newtype GemCollection = GemCollection { unGemCollection :: Map TokenType Int }
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype (ToJSON, FromJSON)

emptyGems :: GemCollection
emptyGems = GemCollection Map.empty

singleGem :: TokenType -> Int -> GemCollection
singleGem t n
  | n == 0    = emptyGems
  | otherwise = GemCollection (Map.singleton t n)

addGems :: GemCollection -> GemCollection -> GemCollection
addGems (GemCollection a) (GemCollection b) =
  GemCollection $ Map.filter (/= 0) $ Map.unionWith (+) a b

subtractGems :: GemCollection -> GemCollection -> GemCollection
subtractGems (GemCollection a) (GemCollection b) =
  GemCollection $ Map.filter (/= 0) $ Map.unionWith (\x y -> x - y) a b

gemCount :: GemCollection -> TokenType -> Int
gemCount (GemCollection m) t = Map.findWithDefault 0 t m

totalGems :: GemCollection -> Int
totalGems (GemCollection m) = sum (Map.elems m)

hasEnoughGems :: GemCollection -> GemCollection -> Bool
hasEnoughGems available required =
  all (\(t, n) -> gemCount available t >= n) (Map.toList (unGemCollection required))

nonNegative :: GemCollection -> Bool
nonNegative (GemCollection m) = all (>= 0) (Map.elems m)

-- | Extract just the gem colors (no gold) from a collection
gemColors :: GemCollection -> Map GemColor Int
gemColors (GemCollection m) =
  Map.fromList [ (c, n) | (GemToken c, n) <- Map.toList m, n > 0 ]
