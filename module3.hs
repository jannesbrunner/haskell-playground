-- Building an interval map data structure in haskell

data IntervalMap k v = IntervalMap {keys :: [k] , values :: [v]} |Empty deriving Show
-- k = key, Typ variable 
-- v = value, Typ variable


singleton :: (Enum k, Num k) => v -> IntervalMap k v
singleton v = IntervalMap{keys=[0..], values= repeat v}

-- get operator => a ! 5 = value at position 5
(!) :: Ord k => IntervalMap k v -> k -> v
(!) iMap k = snd (head (filter (\(x, y) -> x == k) (zip (keys iMap) (values iMap)) ))
