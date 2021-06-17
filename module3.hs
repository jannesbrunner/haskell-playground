-- Building an interval map data structure in haskell

data IntervalMap k v = IntervalMap {keys :: [k] , values :: [v]} |Empty deriving Show
-- k = key, Typ variable 
-- v = value, Typ variable


singleton :: (Enum k, Num k) => v -> IntervalMap k v
singleton v = IntervalMap{keys=[0..], values= repeat v}
