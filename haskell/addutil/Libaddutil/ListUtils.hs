module Libaddutil.ListUtils
where

import qualified Data.Map
import Data.Maybe

qSortList :: Ord a => [a] -> [a]
qSortList [] = []
qSortList (x:xs) = 
    qSortList (filter (< x) xs) ++ 
    [x] ++ 
    qSortList (filter (>= x) xs)

removeDups :: Eq a => [a] -> [a]
removeDups []     = []
removeDups (x:xs) = if elem x xs then 
                        removeDups xs
                    else 
                        x : removeDups xs

uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq (x:xs) = if length xs == 0 then [x] else if x == head xs then uniq xs else x : uniq xs

removeItems :: Eq a => [a] -> [Int] -> [a]
removeItems d []     = d
removeItems d [n]    = deleteFromList d n
removeItems d (n:ns) = removeItems (deleteFromList d (last ns)) (n : init ns)

deleteFromList :: [a] -> Int -> [a]
deleteFromList [] _     = []
deleteFromList l n | n < 1         = l
                   | n == 1        = tail l
                   | n <= length l = (head l) : deleteFromList (tail l) (n - 1)
                   | otherwise     = l

filterByKey :: Ord k => (k -> Bool) -> Data.Map.Map k a -> Data.Map.Map k a
filterByKey p m = Data.Map.filterWithKey (\k _ -> p k) m

makePowerOfTwo :: Int -> Int
makePowerOfTwo n = if isPowerOfTwo n then n else (n + (nextPowerOfTwo n))

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = if n <= 0 then False else isPowerOfTwo' n 1
    where isPowerOfTwo' n' m = if n' - m == 0 then True else if n' > m then isPowerOfTwo' n' (m * 2) else False

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n = if n > head sq then nextPowerOfTwo' n (tail sq) else (head sq)
    where sq = map powPair (zip [2,2..] [1,2..])
          nextPowerOfTwo' n' (p:ps) = if n' > p then nextPowerOfTwo' n' ps else p
          nextPowerOfTwo' _  []     = 2
          powPair (a, b) = a ^ b

findList :: Ord k => [k] -> Data.Map.Map k a -> [a]
findList [] _     = []
findList (k:ks) m = Data.Map.lookup k m ++ findList ks m

insertMany :: Ord k => [k] -> a -> Data.Map.Map k a -> Data.Map.Map k a
insertMany []     _ m = m
insertMany (k:ks) a m = insertMany ks a (Data.Map.insert k a m)

lookupMany :: Ord k => Data.Map.Map k a -> [k] -> [a]
lookupMany _ []     = []
lookupMany m (k:ks) = this ++ rest
    where this       = if isNothing thislookup then [] else (fromJust thislookup) : []
          thislookup = Data.Map.lookup k m
          rest       = lookupMany m ks

findFromPairs :: Eq a => [(a, b)] -> a -> Maybe b
findFromPairs []     _ = Nothing
findFromPairs (x:xs) n = if fst x == n then Just (snd x) else findFromPairs xs n

getMinSndPair :: Ord b => [(a, b)] -> (a, b)
getMinSndPair ps = foldl minSndPair (head ps) (tail ps)

getMaxSndPair :: Ord b => [(a, b)] -> (a, b)
getMaxSndPair ps = foldl maxSndPair (head ps) (tail ps)

maxFstPair :: Ord a => (a, b) -> (a, b) -> (a, b)
maxFstPair p1 p2 = if fst p1 > fst p2 then p1 else p2

maxSndPair :: Ord b => (a, b) -> (a, b) -> (a, b)
maxSndPair p1 p2 = if snd p1 > snd p2 then p1 else p2

minFstPair :: Ord a => (a, b) -> (a, b) -> (a, b)
minFstPair p1 p2 = if fst p1 < fst p2 then p1 else p2

minSndPair :: Ord b => (a, b) -> (a, b) -> (a, b)
minSndPair p1 p2 = if snd p1 < snd p2 then p1 else p2

mapPreserve :: (a -> b) -> [a] -> [(a, b)]
mapPreserve f l = zip l (map f l)

mapPreserveMax :: Ord b => (a -> b) -> [a] -> (a, b)
mapPreserveMax f l = getMaxSndPair $ zip l (map f l)

mapPreserveMin :: Ord b => (a -> b) -> [a] -> (a, b)
mapPreserveMin f l = getMinSndPair $ zip l (map f l)

mapPreserveMaxMaybes :: Ord b => (a -> Maybe b) -> [a] -> (a, b)
mapPreserveMaxMaybes f l = getMaxSndPair $ zip l (mapMaybe f l)

mapPreserveMinMaybes :: Ord b => (a -> Maybe b) -> [a] -> (a, b)
mapPreserveMinMaybes f l = getMinSndPair $ zip l (mapMaybe f l)

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

catLefts :: [Either a b] -> [a]
catLefts []     = []
catLefts (n:ns) = case n of
                    Left  w -> (w : catLefts ns)
                    Right _ -> catLefts ns

catRights :: [Either a b] -> [b]
catRights []     = []
catRights (n:ns) = case n of
                     Left  _ -> catRights ns
                     Right w -> (w : catRights ns)
