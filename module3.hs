-- Building an interval map data structure in haskell

data IntervalMap k v = IntervalMap {keys :: [k] , values :: [v]} |Empty deriving Show
-- k = key, Typ variable 
-- v = value, Typ variable


singleton :: (Enum k, Num k) => v -> IntervalMap k v
singleton v = IntervalMap{keys=[0..], values= repeat v}

-- get operator => a ! 5 = value at position 5
(!) :: Ord k => IntervalMap k v -> k -> v
(!) iMap k = snd (head (filter (\(x, y) -> x == k) (zip (keys iMap) (values iMap)) ))

-- insert a sequence into intervalMap
insert :: (Ord k, Num k, Enum k) => k -> k -> v -> IntervalMap k v -> IntervalMap k v
insert start end value iMap = IntervalMap {keys=keys iMap, values = rangeChanger (values iMap) start end value}

-- helper function to change a range of values in an intervalMap
rangeChanger :: (Num a1, Enum a1, Ord a1) => [a2] -> a1 -> a1 -> a2 -> [a2]
rangeChanger iMapValues start end value = [if (i >= start) && (i <= end) then newValue else iMapValue | (iMapValue, newValue, i) <- zip3 iMapValues (repeat value) [0..]]


