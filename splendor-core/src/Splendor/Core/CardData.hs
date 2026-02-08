module Splendor.Core.CardData
  ( allCards
  , tier1Cards
  , tier2Cards
  , tier3Cards
  , allNobles
  ) where

import Data.Map.Strict qualified as Map
import Splendor.Core.Types.Card (Card(..), Tier(..))
import Splendor.Core.Types.Gem
import Splendor.Core.Types.Noble (Noble(..))

-- Helper to build a cost GemCollection from a list of (GemColor, Int)
cost :: [(GemColor, Int)] -> GemCollection
cost = GemCollection . Map.fromList . map (\(c, n) -> (GemToken c, n)) . filter (\(_, n) -> n > 0)

allCards :: [Card]
allCards = tier1Cards ++ tier2Cards ++ tier3Cards

-- ============================================================
-- TIER 1 CARDS (40 cards)
-- CSV: Level,Color,PV,Black,Blue,Green,Red,White
-- ============================================================
tier1Cards :: [Card]
tier1Cards =
  -- Onyx bonus (8 cards) — CSV Color=Black
  [ Card "t1-o01" Tier1 (cost [(Sapphire, 1), (Emerald, 1), (Ruby, 1), (Diamond, 1)]) Onyx 0
  , Card "t1-o02" Tier1 (cost [(Sapphire, 2), (Emerald, 1), (Ruby, 1), (Diamond, 1)]) Onyx 0
  , Card "t1-o03" Tier1 (cost [(Sapphire, 2), (Ruby, 1), (Diamond, 2)])               Onyx 0
  , Card "t1-o04" Tier1 (cost [(Onyx, 1), (Emerald, 1), (Ruby, 3)])                   Onyx 0
  , Card "t1-o05" Tier1 (cost [(Emerald, 2), (Ruby, 1)])                               Onyx 0
  , Card "t1-o06" Tier1 (cost [(Emerald, 2), (Diamond, 2)])                            Onyx 0
  , Card "t1-o07" Tier1 (cost [(Emerald, 3)])                                          Onyx 0
  , Card "t1-o08" Tier1 (cost [(Sapphire, 4)])                                         Onyx 1
  -- Sapphire bonus (8 cards) — CSV Color=Blue
  , Card "t1-s01" Tier1 (cost [(Onyx, 1), (Emerald, 1), (Ruby, 1), (Diamond, 1)])     Sapphire 0
  , Card "t1-s02" Tier1 (cost [(Onyx, 1), (Emerald, 1), (Ruby, 2), (Diamond, 1)])     Sapphire 0
  , Card "t1-s03" Tier1 (cost [(Emerald, 2), (Ruby, 2), (Diamond, 1)])                Sapphire 0
  , Card "t1-s04" Tier1 (cost [(Sapphire, 1), (Emerald, 3), (Ruby, 1)])               Sapphire 0
  , Card "t1-s05" Tier1 (cost [(Onyx, 2), (Diamond, 1)])                               Sapphire 0
  , Card "t1-s06" Tier1 (cost [(Onyx, 2), (Emerald, 2)])                               Sapphire 0
  , Card "t1-s07" Tier1 (cost [(Onyx, 3)])                                             Sapphire 0
  , Card "t1-s08" Tier1 (cost [(Ruby, 4)])                                             Sapphire 1
  -- Diamond bonus (8 cards) — CSV Color=White
  , Card "t1-d01" Tier1 (cost [(Onyx, 1), (Sapphire, 1), (Emerald, 1), (Ruby, 1)])    Diamond 0
  , Card "t1-d02" Tier1 (cost [(Onyx, 1), (Sapphire, 1), (Emerald, 2), (Ruby, 1)])    Diamond 0
  , Card "t1-d03" Tier1 (cost [(Onyx, 1), (Sapphire, 2), (Emerald, 2)])               Diamond 0
  , Card "t1-d04" Tier1 (cost [(Onyx, 1), (Sapphire, 1), (Diamond, 3)])               Diamond 0
  , Card "t1-d05" Tier1 (cost [(Ruby, 2), (Diamond, 1)])                               Diamond 0
  , Card "t1-d06" Tier1 (cost [(Emerald, 2), (Diamond, 2)])                             Diamond 0
  , Card "t1-d07" Tier1 (cost [(Sapphire, 3)])                                         Diamond 0
  , Card "t1-d08" Tier1 (cost [(Emerald, 4)])                                          Diamond 1
  -- Emerald bonus (8 cards) — CSV Color=Green
  , Card "t1-e01" Tier1 (cost [(Onyx, 1), (Sapphire, 1), (Ruby, 1), (Diamond, 1)])    Emerald 0
  , Card "t1-e02" Tier1 (cost [(Onyx, 2), (Sapphire, 1), (Ruby, 1), (Diamond, 1)])    Emerald 0
  , Card "t1-e03" Tier1 (cost [(Onyx, 2), (Sapphire, 1), (Ruby, 2)])                  Emerald 0
  , Card "t1-e04" Tier1 (cost [(Sapphire, 3), (Emerald, 1), (Diamond, 1)])             Emerald 0
  , Card "t1-e05" Tier1 (cost [(Sapphire, 1), (Diamond, 2)])                           Emerald 0
  , Card "t1-e06" Tier1 (cost [(Sapphire, 2), (Ruby, 2)])                              Emerald 0
  , Card "t1-e07" Tier1 (cost [(Ruby, 3)])                                             Emerald 0
  , Card "t1-e08" Tier1 (cost [(Onyx, 4)])                                             Emerald 1
  -- Ruby bonus (8 cards) — CSV Color=Red
  , Card "t1-r01" Tier1 (cost [(Onyx, 1), (Sapphire, 1), (Emerald, 1), (Diamond, 1)]) Ruby 0
  , Card "t1-r02" Tier1 (cost [(Onyx, 1), (Sapphire, 1), (Emerald, 1), (Diamond, 2)]) Ruby 0
  , Card "t1-r03" Tier1 (cost [(Onyx, 2), (Emerald, 1), (Diamond, 2)])                Ruby 0
  , Card "t1-r04" Tier1 (cost [(Onyx, 3), (Ruby, 1), (Diamond, 1)])                    Ruby 0
  , Card "t1-r05" Tier1 (cost [(Sapphire, 2), (Emerald, 1)])                           Ruby 0
  , Card "t1-r06" Tier1 (cost [(Ruby, 2), (Diamond, 2)])                               Ruby 0
  , Card "t1-r07" Tier1 (cost [(Diamond, 3)])                                          Ruby 0
  , Card "t1-r08" Tier1 (cost [(Diamond, 4)])                                          Ruby 1
  ]

-- ============================================================
-- TIER 2 CARDS (30 cards)
-- CSV: Level,Color,PV,Black,Blue,Green,Red,White
-- ============================================================
tier2Cards :: [Card]
tier2Cards =
  -- Onyx bonus (6 cards) — CSV Color=Black
  [ Card "t2-o01" Tier2 (cost [(Sapphire, 2), (Emerald, 2), (Diamond, 3)])            Onyx 1
  , Card "t2-o02" Tier2 (cost [(Onyx, 2), (Emerald, 3), (Diamond, 3)])                Onyx 1
  , Card "t2-o03" Tier2 (cost [(Sapphire, 1), (Emerald, 4), (Ruby, 2)])               Onyx 2
  , Card "t2-o04" Tier2 (cost [(Emerald, 5), (Ruby, 3)])                               Onyx 2
  , Card "t2-o05" Tier2 (cost [(Diamond, 5)])                                           Onyx 2
  , Card "t2-o06" Tier2 (cost [(Onyx, 6)])                                             Onyx 3
  -- Sapphire bonus (6 cards) — CSV Color=Blue
  , Card "t2-s01" Tier2 (cost [(Sapphire, 2), (Emerald, 2), (Ruby, 3)])               Sapphire 1
  , Card "t2-s02" Tier2 (cost [(Onyx, 3), (Sapphire, 2), (Emerald, 3)])               Sapphire 1
  , Card "t2-s03" Tier2 (cost [(Sapphire, 3), (Diamond, 5)])                           Sapphire 2
  , Card "t2-s04" Tier2 (cost [(Onyx, 4), (Ruby, 1), (Diamond, 2)])                   Sapphire 2
  , Card "t2-s05" Tier2 (cost [(Sapphire, 5)])                                         Sapphire 2
  , Card "t2-s06" Tier2 (cost [(Sapphire, 6)])                                         Sapphire 3
  -- Diamond bonus (6 cards) — CSV Color=White
  , Card "t2-d01" Tier2 (cost [(Onyx, 2), (Emerald, 3), (Ruby, 2)])                   Diamond 1
  , Card "t2-d02" Tier2 (cost [(Sapphire, 3), (Ruby, 3), (Diamond, 2)])               Diamond 1
  , Card "t2-d03" Tier2 (cost [(Onyx, 2), (Emerald, 1), (Ruby, 4)])                   Diamond 2
  , Card "t2-d04" Tier2 (cost [(Onyx, 3), (Ruby, 5)])                                 Diamond 2
  , Card "t2-d05" Tier2 (cost [(Ruby, 5)])                                             Diamond 2
  , Card "t2-d06" Tier2 (cost [(Diamond, 6)])                                          Diamond 3
  -- Emerald bonus (6 cards) — CSV Color=Green
  , Card "t2-e01" Tier2 (cost [(Emerald, 2), (Ruby, 3), (Diamond, 3)])                Emerald 1
  , Card "t2-e02" Tier2 (cost [(Onyx, 2), (Sapphire, 3), (Diamond, 2)])               Emerald 1
  , Card "t2-e03" Tier2 (cost [(Onyx, 1), (Sapphire, 2), (Diamond, 4)])               Emerald 2
  , Card "t2-e04" Tier2 (cost [(Sapphire, 5), (Emerald, 3)])                           Emerald 2
  , Card "t2-e05" Tier2 (cost [(Emerald, 5)])                                          Emerald 2
  , Card "t2-e06" Tier2 (cost [(Emerald, 6)])                                          Emerald 3
  -- Ruby bonus (6 cards) — CSV Color=Red
  , Card "t2-r01" Tier2 (cost [(Onyx, 3), (Diamond, 2), (Ruby, 2)])                   Ruby 1
  , Card "t2-r02" Tier2 (cost [(Onyx, 3), (Sapphire, 3), (Ruby, 2)])                  Ruby 1
  , Card "t2-r03" Tier2 (cost [(Sapphire, 4), (Emerald, 2), (Diamond, 1)])            Ruby 2
  , Card "t2-r04" Tier2 (cost [(Onyx, 5), (Diamond, 3)])                               Ruby 2
  , Card "t2-r05" Tier2 (cost [(Onyx, 5)])                                             Ruby 2
  , Card "t2-r06" Tier2 (cost [(Ruby, 6)])                                             Ruby 3
  ]

-- ============================================================
-- TIER 3 CARDS (20 cards)
-- CSV: Level,Color,PV,Black,Blue,Green,Red,White
-- ============================================================
tier3Cards :: [Card]
tier3Cards =
  -- Onyx bonus (4 cards) — CSV Color=Black
  [ Card "t3-o01" Tier3 (cost [(Sapphire, 3), (Emerald, 5), (Ruby, 3), (Diamond, 3)]) Onyx 3
  , Card "t3-o02" Tier3 (cost [(Ruby, 7)])                                              Onyx 4
  , Card "t3-o03" Tier3 (cost [(Onyx, 3), (Emerald, 3), (Ruby, 6)])                   Onyx 4
  , Card "t3-o04" Tier3 (cost [(Onyx, 3), (Ruby, 7)])                                  Onyx 5
  -- Sapphire bonus (4 cards) — CSV Color=Blue
  , Card "t3-s01" Tier3 (cost [(Onyx, 5), (Emerald, 3), (Ruby, 3), (Diamond, 3)])     Sapphire 3
  , Card "t3-s02" Tier3 (cost [(Diamond, 7)])                                          Sapphire 4
  , Card "t3-s03" Tier3 (cost [(Onyx, 3), (Sapphire, 3), (Diamond, 6)])               Sapphire 4
  , Card "t3-s04" Tier3 (cost [(Sapphire, 3), (Diamond, 7)])                           Sapphire 5
  -- Diamond bonus (4 cards) — CSV Color=White
  , Card "t3-d01" Tier3 (cost [(Onyx, 3), (Sapphire, 3), (Emerald, 3), (Ruby, 5)])    Diamond 3
  , Card "t3-d02" Tier3 (cost [(Onyx, 7)])                                              Diamond 4
  , Card "t3-d03" Tier3 (cost [(Onyx, 6), (Ruby, 3), (Diamond, 3)])                   Diamond 4
  , Card "t3-d04" Tier3 (cost [(Onyx, 7), (Diamond, 3)])                               Diamond 5
  -- Emerald bonus (4 cards) — CSV Color=Green
  , Card "t3-e01" Tier3 (cost [(Onyx, 3), (Sapphire, 3), (Ruby, 3), (Diamond, 5)])    Emerald 3
  , Card "t3-e02" Tier3 (cost [(Sapphire, 7)])                                         Emerald 4
  , Card "t3-e03" Tier3 (cost [(Sapphire, 6), (Emerald, 3), (Diamond, 3)])            Emerald 4
  , Card "t3-e04" Tier3 (cost [(Sapphire, 7), (Emerald, 3)])                           Emerald 5
  -- Ruby bonus (4 cards) — CSV Color=Red
  , Card "t3-r01" Tier3 (cost [(Onyx, 3), (Sapphire, 5), (Emerald, 3), (Diamond, 3)]) Ruby 3
  , Card "t3-r02" Tier3 (cost [(Emerald, 7)])                                          Ruby 4
  , Card "t3-r03" Tier3 (cost [(Sapphire, 3), (Emerald, 6), (Ruby, 3)])               Ruby 4
  , Card "t3-r04" Tier3 (cost [(Emerald, 7), (Ruby, 3)])                               Ruby 5
  ]

-- ============================================================
-- NOBLES (10 nobles, each worth 3 prestige)
-- ============================================================
allNobles :: [Noble]
allNobles =
  -- 4-4 nobles
  [ Noble "n01" (Map.fromList [(Diamond, 4), (Sapphire, 4)])              3
  , Noble "n02" (Map.fromList [(Sapphire, 4), (Emerald, 4)])              3
  , Noble "n03" (Map.fromList [(Emerald, 4), (Ruby, 4)])                  3
  , Noble "n04" (Map.fromList [(Ruby, 4), (Onyx, 4)])                     3
  , Noble "n05" (Map.fromList [(Onyx, 4), (Diamond, 4)])                  3
  -- 3-3-3 nobles
  , Noble "n06" (Map.fromList [(Diamond, 3), (Sapphire, 3), (Emerald, 3)]) 3
  , Noble "n07" (Map.fromList [(Sapphire, 3), (Emerald, 3), (Ruby, 3)])    3
  , Noble "n08" (Map.fromList [(Emerald, 3), (Ruby, 3), (Onyx, 3)])        3
  , Noble "n09" (Map.fromList [(Diamond, 3), (Ruby, 3), (Onyx, 3)])        3
  , Noble "n10" (Map.fromList [(Diamond, 3), (Sapphire, 3), (Onyx, 3)])    3
  ]
