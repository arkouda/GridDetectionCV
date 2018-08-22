import qualified Data.Vector as V
-- use mutable vectors and uboxed preferably
import Data.Either (fromRight, partitionEithers)
import Codec.Picture
import Data.List (groupBy,nub,(\\))
import qualified Data.HashMap as H

-- toGreyScaleMatrix :: String -> Vector (Vector (Int,Int,Int))
toGreyScaleMatrix s = groupBy (\(a1,a2,a3) (b1,b2,b3)-> (a1 +1) /= b1) $ zipWith (\(x,y) z-> (x,y,z)) l1 s4
  where
    s1 = drop 2 $ lines s
    s2 = map (\x-> read x :: Int) $ drop 2 s1
    s3 = zip [0,1..] s2
    lw = (\[x,y]-> (\(x,y)->(x,y)) $ (read x :: Int, read y :: Int)) $ words $ head s1
    l1 = concat $ map (\x-> zip (repeat x) [0..(fst lw - 1)]) [0..(snd lw -1)]
    getIndices3 n = map (\(x,y)-> y) $ filter (\(x,y)-> rem x 3 == n) $ s3
    s4 = zip3 (getIndices3 0) (getIndices3 1) (getIndices3 2)

clamp mn mx val = max mn (min val mx)

matrixUpdate :: (a -> a) -> (Int,Int) -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
matrixUpdate f pos matrix = V.accum (\x y-> V.accum (\a b-> b a) x [(snd pos,y)]) matrix [(fst pos, f)]

threshold (r,g,b) (r1,g1,b1) = (<) 220 $ abs $ (r+g+b)-(r1+g1+b1)
isDarker (r,g,b) (r1,g1,b1) = (<) (div (r+g+b) 3) (div (r1+g1+b1) 3)

getEdgeIndices :: [[(Int,Int,(Int,Int,Int))]] -> [(Int,Int)]
getEdgeIndices s = concat.map (concat.map isDisc) $ s
  where
    (mr,mc) = (length s,length $ head s)
    isDisc (rw,cl,p1@(r,g,b)) = map (\(x,y,p)-> if isDarker p p1 then (x,y) else (rw,cl)) $ filter (\(_,_,p2)-> threshold p1 p2 ) $ neighIndicesWP rw cl
    neighIndicesWP rw cl = map (\(x,y) -> (s!!x)!!y) $ neighIndices rw cl
    neighIndices rw cl = filter (\(x,y)-> x<mr && x>=0 && y<mc && y>=0) [(rw-1,cl),(rw+1,cl),(rw,cl-1),(rw,cl+1),(rw-1,cl-1),(rw+1,cl+1),(rw-1,cl+1),(rw+1,cl-1)]

getNeigh :: (Int,Int) -> (Int,Int) -> H.Map (Int,Int) Int -> [(Int,Int)]
getNeigh pos prevPos hm = take 1 $ filter (\x->(H.member x hm) && (H.lookup x hm == Just 1)) directionList
  where
    directionalPoints = [(rw-1,cl-1),(rw-1,cl),(rw-1,cl+1),(rw,cl+1),(rw+1,cl+1),(rw+1,cl),(rw+1,cl-1),(rw,cl-1)]
    rw = fst pos
    cl = snd pos
    relativeDirection = getRelativeDirection pos prevPos
    d1 = (relativeDirection + 3)
    p1Direction = (\[x,y,z]-> [y,x,z]) $ take 3 $ drop d1 $ [0..7] ++ [0..7]
    p2Direction = [directionCircle!!(8+(p1Direction!!1) -1), directionCircle!!(p1Direction!!2 +1)]
    p3Direction = [directionCircle!!(8+(p2Direction!!0) -1), directionCircle!!(p2Direction!!1 +1)]
    directionList = map (\x-> directionalPoints!!x) $ p1Direction ++ p2Direction ++ p3Direction
    directionCircle = concat $ replicate 3 [0..7]

getRelativeDirection (rw,cl) prevPos | (rw-1,cl-1) == prevPos = 0
                                     | (rw-1,cl) == prevPos = 1
                                     | (rw-1,cl+1) == prevPos = 2
                                     | (rw,cl+1) == prevPos = 3
                                     | (rw+1,cl+1) == prevPos = 4
                                     | (rw+1,cl) == prevPos = 5
                                     | (rw+1,cl-1) == prevPos = 6
                                     | (rw,cl-1) == prevPos = 7
                                     | otherwise = 0

dfsEdge :: [(Int,Int)] -> [(Int,Int)]
dfsEdge [] = []
dfsEdge xs = recDFSEdge hashMap [(head xs)] []
  where
    hashMap = H.fromList $ zip xs (repeat 1)

recDFSEdge hm [] xs = xs
recDFSEdge hm tup_list xs = recDFSEdge hm1 new_tup_list (xs ++ tup_list)
  where
    hm1 = H.adjust (\a-> 0) (head tup_list) hm
    new_tup_list = getNeigh (head tup_list) (if null xs then (0,0) else last xs) hm1

---------------------------- commented section -------------------------------

getNeighBFS :: (Int,Int) -> H.Map (Int,Int) Int -> [(Int,Int)]
getNeighBFS pos hm = filter (\x->(H.member x hm) && (H.lookup x hm == Just 1)) [(rw-1,cl),(rw+1,cl),(rw,cl-1),(rw,cl+1),(rw-1,cl-1),(rw+1,cl+1),(rw-1,cl+1),(rw+1,cl-1)]
  where
    rw = fst pos
    cl = snd pos

bfsEdge :: [(Int,Int)] -> [(Int,Int)]
bfsEdge xs = recBFSEdge hashMap [(head xs)] []
  where
    hashMap = H.fromList $ zip xs (repeat 1)

recBFSEdge hm [] xs = xs
recBFSEdge hm tup_list xs = recBFSEdge hm1 new_tup_list (xs ++ tup_list)
  where
    hm1 = foldl (\b t-> H.adjust (\a-> 0) t b) hm tup_list
    new_tup_list = nub $ concat $ map (\x-> getNeighBFS x hm1) tup_list

