{-

Name:Hima Bindu Krovvidi
Collaborators/Acknowledgements:

-}

module CardGames
  ( Rank (..),
    Suit (..),
    Card (..),
    rank,
    suit,
    deck,
    pokerHandCompare,
    cribbageScoreTheShow,
  )
where

import Data.Either qualified as Either
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord

-- ###################################################################
-- ###################################################################

-- Cards

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Eq, Enum, Bounded, Read, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Enum, Bounded, Read, Show)

data Card = Rank :@: Suit
  deriving (Eq, Read, Show)

rank :: Card -> Rank
rank (r :@: _) = r

suit :: Card -> Suit
suit (_ :@: s) = s

-- ###################################################################

-- Deck

deck :: [Card]
deck = [r :@: s | r <- [minBound .. maxBound], s <- [minBound .. maxBound]]

-- ###################################################################

-- Poker

pokerHandCompare :: (Card, Card, Card, Card, Card) -> (Card, Card, Card, Card, Card) -> Maybe Ordering
pokerHandCompare (c1,c2,c3,c4,c5) (c6,c7,c8,c9,c10) =
  let deck1 = [c1, c2, c3, c4, c5]
      deck2 = [c6, c7, c8, c9, c10]
   in if length (List.nub deck2) /= 5 || length (List.nub deck1) /= 5
        then Nothing
        else
          if Just (Ord.compare (calculatePoints deck1) (calculatePoints deck2)) /= Just EQ
          then Just (Ord.compare (calculatePoints deck1) (calculatePoints deck2))
          else Just (find (sortBy (flip rankCom) (map rank deck1)) (sortBy (flip rankCom) (map rank deck2)))


find :: [Rank] -> [Rank] -> Ordering
find [] [] = EQ
find _ [] = GT
find [] _ = LT
find (x:xs) (y:ys) = case length(List.nub (x:xs)) of
  3 -> if go==EQ then (if go1==EQ then lastCheck (x:xs) (y:ys) else go1) else go
    where go = Ord.compare (multipleElem (map pokerGameEnum (x:xs))) (multipleElem (map pokerGameEnum (y:ys)))
          go1 = Ord.compare (multipleElem (filter (/=multipleElem (map pokerGameEnum (x:xs))) (map pokerGameEnum (x:xs)))) (multipleElem (filter (/=multipleElem (map pokerGameEnum (y:ys))) (map pokerGameEnum (y:ys))))
  5 -> lastCheck (x:xs) (y:ys)
  _ -> if go == EQ then lastCheck (x:xs) (y:ys) else go
    where go = Ord.compare (multipleElem (map pokerGameEnum (x:xs))) (multipleElem (map pokerGameEnum (y:ys)))

multipleElem :: Ord a => [a] -> a
multipleElem = snd.maximum.map (\xs -> (length xs, head xs)).List.group.List.sort

lastCheck :: [Rank] -> [Rank] -> Ordering
lastCheck [] [] = EQ
lastCheck [] _ = LT
lastCheck _ [] = GT
lastCheck [x] [y] = if x == y then EQ else rankCom x y
lastCheck (x1:x2:xs) (y1:y2:ys)
  | x1 == y1 = lastCheck (x2:xs) (y2:ys)
  | checkStraight (reverse (x1:x2 : xs))
      && checkStraight (reverse (y1:y2 : ys)) && (x1 == Ace || y1 == Ace) && (x2/=King && y2/=King)
      = if x1 == Ace then LT else GT
  | otherwise = rankCom x1 y1
lastCheck _ _ = EQ

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:xs) = sortBy f (filter (\g -> f g x == LT) xs) ++ [x] ++ sortBy f (filter (\g -> f g x /= LT) xs)

checkStraight :: [Rank] -> Bool
checkStraight [] = True
checkStraight [_] = True
checkStraight [Two,Three,Four,Five,Ace] = True
checkStraight (x:y:xs) = ((rankCom x y == LT) && (pokerGameEnum y - pokerGameEnum x == 1)) && checkStraight (y:xs)

calculatePoints :: [Card] -> Int
calculatePoints hand =
  let ranks = sortBy rankCom (map rank hand)
      suits = map suit hand
      straight = checkStraight ranks
      flush = length (List.nub suits) == 1

      currCounts = List.sort (map length (List.group ranks))
      r1 = head currCounts
      r2 = head (tail currCounts)
      r3 = head (tail (tail currCounts))
      r4 = head (tail (tail (tail currCounts)))
      r5 = head (tail (tail (tail (tail currCounts))))

   in if straight && flush then 9
      else if r1 == 1 && r2 == 4 then 8
      else if r1 == 2 && r2 == 3 then 7
      else if flush && not straight then 6
      else if straight && not flush then 5
      else if r1 == 1 && r2 == 1 && r3 == 3 then 4
      else if r1 == 1 && r2 == 2 && r3 == 2 then 3
      else if r1 == 1 && r2 == 1 && r3 == 1 && r4 == 2 then 2
      else if r1 == 1 && r2 == 1 && r3 == 1 && r4 == 1 && r5 == 1 && not straight && not flush then 1 else 0

rankCom :: Rank -> Rank -> Ordering
rankCom r1 r2 =
  if r1 == r2 then
    EQ
  else
    Ord.comparing pokerGameEnum r1 r2

pokerGameEnum :: Rank -> Int
pokerGameEnum Two = 2
pokerGameEnum Three = 3
pokerGameEnum Four = 4
pokerGameEnum Five = 5
pokerGameEnum Six = 6
pokerGameEnum Seven = 7
pokerGameEnum Eight = 8
pokerGameEnum Nine = 9
pokerGameEnum Ten = 10
pokerGameEnum Jack = 11
pokerGameEnum Queen = 12
pokerGameEnum King = 13
pokerGameEnum Ace = 14

-- ###################################################################

cribbageScoreTheShow :: Card -> Bool -> (Card, Card, Card, Card) -> Maybe Int
cribbageScoreTheShow starter crib (c1,c2,c3,c4) =
  let hand = [starter,c1,c2,c3,c4]
  in if length (List.nub hand) /= 5 then Nothing else Just (cribbageScoreHand hand crib)

cribbageScoreHand :: [Card] -> Bool -> Int
cribbageScoreHand hand crib = fifteens ranks + noOfRuns ranks + pairs hand + flushes hand crib + nobs hand
  where ranks = cribbageSortBy cribbageRankCompare (map rank hand)

fifteens :: [Rank] -> Int
fifteens xs = 2 * foldr (\y c -> if sum [if z `elem` [King,Queen,Jack] then 10 
  else cribbageEnum z | z <- y] == 15 then c+1 else c) 0 (generatesubseqs (length xs) xs)

noOfRuns :: [Rank] -> Int
noOfRuns [] = 0
noOfRuns [_] = 0
noOfRuns xs 
  | t < 3 = 0 
  | otherwise = t * foldr (\y ys -> if y == t then ys+1 else ys) 0 [length x | x <- generatesubseqs (length xs) xs , isStraight x]
    where t = maximum  [ length x | x <- generatesubseqs (length xs) xs , isStraight x]

isStraight :: [Rank] -> Bool
isStraight [] = False
isStraight x = straightHelper (cribbageSortBy cribbageRankCompare x)

straightHelper :: [Rank] -> Bool
straightHelper [] = True
straightHelper [_] = True
straightHelper (x:y:xs) =  (cribbageEnum x + 1 == cribbageEnum y ) && straightHelper (y:xs)

pairs :: [Card] -> Int
pairs hand
  | count == [1, 4, 4, 4, 4] = 12
  | count == [1, 1, 3, 3, 3] = 6
  | count == [1, 2, 2, 2, 2] = 4
  | count == [2, 2, 3, 3, 3] = 8
  | count == [1, 1, 1, 2, 2] = 2
  | otherwise = 0
    where count = List.sort(map snd ( calPairs (map rank hand)))

calPairs :: [Rank] -> [(Rank, Int)]
calPairs xs = map (\element -> (element, countElements element xs)) xs

countElements :: Rank -> [Rank] -> Int
countElements x = foldr (\y ys -> if y == x then ys+1 else ys) 0

flushes :: [Card] -> Bool -> Int
flushes h f
  | f && isFlush h = 5
  | not f && isFlush h = 5
  | not f && isFlush xs = 4
  | otherwise = 0
    where (_:xs) = h


isFlush :: [Card] -> Bool
isFlush [] = True
isFlush [_] = True
isFlush (x:y:xs) = (suit x == suit y) && isFlush (y:xs)

nobs :: [Card] -> Int
nobs [] = 0
nobs ((_:@:s1):xs) = if length [r | (r:@:s) <- xs, s == s1 && r == Jack ] == 1 then 1 else 0

cribbageRankCompare :: Rank -> Rank -> Ordering
cribbageRankCompare r1 r2 =
  if r1 == r2 then
    EQ
  else
    Ord.comparing cribbageEnum r1 r2

cribbageSortBy :: (a -> a -> Ordering) -> [a] -> [a]
cribbageSortBy _ [] = []
cribbageSortBy f (x:xs) = sortBy f (filter (\y -> f y x == LT) xs) ++ [x] ++ sortBy f (filter (\y -> f y x /= LT) xs)

cribbageEnum :: Rank -> Int
cribbageEnum Ace = 1
cribbageEnum Two = 2
cribbageEnum Three = 3
cribbageEnum Four = 4
cribbageEnum Five = 5
cribbageEnum Six = 6
cribbageEnum Seven = 7
cribbageEnum Eight = 8
cribbageEnum Nine = 9
cribbageEnum Ten = 10
cribbageEnum Jack = 11
cribbageEnum Queen = 12
cribbageEnum King = 13

subseqs :: Int -> [Rank] -> [[Rank]]
subseqs 0 _ = [[]]
subseqs _ [] = []
subseqs n (x : xs) = map (x :) (subseqs (n - 1) xs) ++ subseqs n xs

generatesubseqs :: Int -> [Rank] -> [[Rank]]
generatesubseqs 0 _ = [[]]
generatesubseqs n xs = subseqs n xs ++ generatesubseqs (n-1) xs
