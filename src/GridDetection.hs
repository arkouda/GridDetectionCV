
import System.Environment (getArgs)
import qualified Graphics.Image as G
import qualified Graphics.Image.Interface as GI
import qualified Graphics.Image.Interface.Vector as GIV
import qualified Data.HashMap as H
import qualified Data.Hashable as Ha
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Statistics.LinearRegression (linearRegressionTLS)
-- use mutable vectors, unboxed preferably
import Data.Either (fromRight, partitionEithers)
import Data.List (groupBy, nub, (\\), sort, sortBy)

edgeSensitivity :: Double
edgeSensitivity = 0.3             -- [0,1]

noiseThreshold :: Int
noiseThreshold = 30               -- nT >= 0

vThreshold = 2
hThreshold = 0.5

data Orientation = Vertical | Horizontal | Suspicious deriving (Eq,Show)

data PixelCluster = PXC { pixelList :: V.Vector (Int,Int),
                          boundingBox :: ((Int,Int),(Int,Int)),
                          vhRatio :: Double,
                          slopeNintersect :: (Double,Double),
                          orientation :: Orientation } deriving (Eq,Show)

toPixelCluster :: [(Int,Int)] -> PixelCluster
toPixelCluster pixList = PXC pixelList boundingBox vhRatio slopeNintersect orientation
  where
    pixelList = V.fromList pixList
    boundingBox = ((\(x,y)-> ((minimum x, minimum y),(maximum x, maximum y))) $ unzip pixList)
    vhRatio = (\((minX,minY),(maxX,maxY))-> (fromIntegral $ (maxY-minY))/(fromIntegral $ (maxX-minX))) $ boundingBox
    slopeNintersect = uncurry linearRegressionTLS (V.unzip $ V.map (\(x,y)->
                                                                      ((fromIntegral x :: Double),(fromIntegral y :: Double))) pixelList)
    orientation = if vhRatio <= hThreshold then Horizontal else (if vhRatio >= vThreshold then Vertical else Suspicious)

main :: IO ()
main = do
  argList <- getArgs
  let imgName = head argList
  origImg <- G.readImageY G.VU imgName
  let origImgM = GI.toManifest origImg  -- Convert to manifest array
      edgeImg = toEdgeImage origImgM
      edgeHM = VU.foldl (\hm (pos,_)-> H.insert pos (1::Int) hm) (H.empty) (GIV.filter (\pixelY-> pixelY == (G.PixelY 1)) edgeImg)
      edgeSetGC = getAllConnected edgeHM
      properSet = filter (\x->(length x)>noiseThreshold) edgeSetGC
      pixelClusterList = map toPixelCluster properSet
      verticalPXC = filter (\pxc-> orientation pxc == Vertical) pixelClusterList
      horizontalPXC = filter (\pxc-> orientation pxc == Horizontal) pixelClusterList
      clusterH = map (\x-> concat $ map (V.toList.pixelList) x) $ getClusters horizontalPXC Horizontal
      clusterV = map (\x-> concat $ map (V.toList.pixelList) x) $ getClusters verticalPXC Vertical
      rectPxList = foldl (\l k-> l ++ (makeRect k)) [] $
                   map (\e-> (\(x,y)-> ((minimum x, minimum y),(maximum x, maximum y))) $
                         unzip e) (clusterV++clusterH++properSet)
      activePixelHM = H.fromList $ zip (concat properSet ++ rectPxList) (repeat 0) :: H.Map (Int,Int) Int
      activePixelImage = G.makeImageR G.VU (G.dims origImg) (\tup-> if (H.member tup activePixelHM)
                                                then (G.PixelY (1::Double))
                                                else (G.PixelY (0::Double)))
  G.writeImage ((takeWhile (/= '.') imgName) ++ "Output.jpg") activePixelImage

distanceThreshold = 60

distanceX pxc1 pxc2 = (\(((minX1,minY1),(maxX1,maxY1)),((minX2,minY2),(maxX2,maxY2)))->
                         minimum [abs (maxX1 - minX2), abs (minX1 - maxX2)]) $
                      (boundingBox pxc1, boundingBox pxc2)

distanceY pxc1 pxc2 = (\(((minX1,minY1),(maxX1,maxY1)),((minX2,minY2),(maxX2,maxY2)))->
                         minimum [abs (maxY1 - minY2), abs (minY1 - maxY2)]) $
                      (boundingBox pxc1, boundingBox pxc2)

getClusters :: [PixelCluster] -> Orientation -> [[PixelCluster]]
getClusters pxcList orientation = getClusters' pxcList orientation []

getClusters' pxcList orientation clusterList = if null pxcList then clusterList
                                               else getClusters' newpxcList orientation (clusterList ++ [newCluster])
  where
    distance pxc1 pxc2 = if orientation == Vertical then distanceX pxc1 pxc2 else distanceY pxc1 pxc2
    newCluster = filter (\pxc-> distance (head pxcList) pxc < distanceThreshold) pxcList
    newpxcList = filter (\pxc-> distance (head pxcList) pxc > distanceThreshold) pxcList



toEdgeImage :: G.Image G.VU G.Y Double -> G.Image G.VU G.Y Double
toEdgeImage origImgM = G.imap (\(r,c) pixelY-> if (any id $
                                  map (\pos -> (>)
                                        (pixelY - (GI.handleBorderIndex (GI.Fill 1) (GI.dims origImgM) (GI.index origImgM) pos))
                                        (G.PixelY edgeSensitivity))
                                   [(r-1,c),(r+1,c),(r,c-1),(r,c+1)])
                              then G.PixelY (1::Double)
                              else G.PixelY (0::Double)) origImgM              -- Convert to bilevel edge Image

makeRect ((xMin,yMin),(xMax,yMax)) = (map (\x-> (x,yMin)) [xMin..xMax])++(map (\x->(x,yMax)) [xMin..xMax])++(map (\y->(xMax,y)) [yMin..yMax])++(map (\y->(xMin,y)) [yMin..yMax])

getAllConnected :: H.Map (Int,Int) Int -> [[(Int,Int)]]
getAllConnected edgeHM = getAllConnected' edgeHM []

getAllConnected' :: H.Map (Int,Int) Int -> [[(Int,Int)]] -> [[(Int,Int)]]
getAllConnected' edgeHM lineList = if (getNewPos edgeHM) == invalidPos
                                   then lineList
                                   else getAllConnected' newEdgeHM (lineList ++ [newLineList])
  where
    (newLineList, newEdgeHM) = getConnected (getNewPos edgeHM) edgeHM []

getConnected :: (Int,Int) -> H.Map (Int,Int) Int -> [(Int,Int)] -> ([(Int,Int)],H.Map (Int,Int) Int)
getConnected currentPos visitedHM connectedPositionsList = if null foundNeighbours
                                                           then (connectedPositionsList, newVisitedHM)
                                                           else (foundNeighbours ++ newConnectedPositionsList, newVisitedHM2)
  where
    rw = fst currentPos
    cl = snd currentPos
    possibleNeighbours = [(rw-1,cl-1),(rw-1,cl),(rw-1,cl+1),(rw,cl+1),(rw+1,cl+1),(rw+1,cl),(rw+1,cl-1),(rw,cl-1)] 
    foundNeighbours = filter (\possiblePos->(H.member possiblePos visitedHM)&&(H.lookup possiblePos visitedHM==(Just 1)))
                      $ possibleNeighbours ++ [currentPos]
    newVisitedHM = foldl (\hm pos'-> H.adjust (\a-> 0) pos' hm) visitedHM foundNeighbours
    (newConnectedPositionsList, newVisitedHM2) = foldl (\(posList, hm) pos'-> (\(pL,h)-> (nub $ (pL ++ posList),h)) $ getConnected pos' hm posList) ([], newVisitedHM) foundNeighbours

getNewPos :: H.Map (Int,Int) Int -> (Int,Int)
getNewPos edgeHM = validPosCheck $ filter (\key->(H.member key edgeHM)&&(H.lookup key edgeHM==Just 1)) $ H.keys edgeHM

invalidPos = (-100,-100)

validPosCheck [] = invalidPos
validPosCheck xs = head xs

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

dfsEdge xs = recDFSEdge hashMap [(head xs)] []
  where
    hashMap = H.fromList $ zip xs (repeat 1)

recDFSEdge hm [] xs = (xs,hm)
recDFSEdge hm tup_list xs = recDFSEdge hm1 new_tup_list (xs ++ tup_list)
  where
    hm1 = H.adjust (\a-> 0) (head tup_list) hm
    new_tup_list = getNeigh (head tup_list) (if null xs then (0,0) else last xs) hm1

mySlice :: Int -> Int -> V.Vector a -> V.Vector a
mySlice s l xs = if (s < 0)
                 then (mySlice (V.length xs + s) l xs)
                 else (if ((s+l) > (V.length xs))
                       then (V.++) (mySlice s (V.length xs - s) xs) (mySlice 0 (l - (V.length xs - s)) xs)
                       else (V.slice s l xs))

fitLine :: V.Vector (Double, Double) -> (Int, Int) -> (Double, Double)
fitLine vec (s,l) = linearRegressionTLS xVec yVec
  where
    newVec = mySlice s l vec
    xVec = fst $ V.unzip newVec
    yVec = snd $ V.unzip newVec

neighbourhoodToConsiderForSlope :: Double
neighbourhoodToConsiderForSlope = 0.3

getSlopePoints :: V.Vector (Int,Int) -> V.Vector (Double,Double)
getSlopePoints xs = V.map (\(s,l)-> fitLine dxs (s,l)) (V.generate (V.length xs) getRange)
  where
    dxs = V.map (\(x,y)-> ((fromIntegral x)::Double, (fromIntegral y)::Double)) xs
    l = V.length xs
    offset = floor $ (fromIntegral l) * (neighbourhoodToConsiderForSlope / 2)
    getRange i = (i-offset, offset * 2)

myClamp i l = if i >= l then i - l else i

getMid s e l = if mid >= l then mid - l else mid
  where
    mid = (if (s > e) then (ceiling $ (fromIntegral (s + e + l - 1)) / 2) else ceiling $ (fromIntegral (s + e)) / 2) :: Int

isAlmostPerpendicular m1 m2 = if (m <= (1.6708)) && (m >= (1.4708)) then True else False
  where
    m = atan $ (m1 - m2) / (1 - m1 * m2)


detectShifts :: V.Vector (Double,Double) -> V.Vector Int
detectShifts slopeIntersectVec = V.imap (\i (c,m)-> if isAlmostPerpendicular m (snd $ (V.!) slopeIntersectVec (myClamp (i+offset) l))
                                                    then (getMid i (myClamp (i+offset) l) l) else -1) slopeIntersectVec
  where
    l = V.length slopeIntersectVec
    offset = floor $ (fromIntegral l) * neighbourhoodToConsiderForSlope


-- SCRATCHED CODE --

--edgeSet = filter (\x->(length x)>10) $ snd $ foldl (\(hm1,lds) _ -> (\(nlds,hm2)->(hm2,(nlds:lds))) $ if (null $ activeEdge hm1) then ([],H.empty) else (recDFSEdge hm1 (activeEdge hm1) [])) (hm,[]) [1..(H.size hm)]


-- newFxn g t = do
--   GI.write g t (G.PixelY (1::Double))
--   return g

-- g2 = G.imap (\(r,c) p-> if (any id $ map (\i -> (>) (p - (GI.handleBorderIndex (GI.Fill 1) (GI.dims g1) (GI.index g1) i)) (G.PixelY 0.5)) [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]) then G.PixelY (1::Double) else G.PixelY (0::Double)) g1

-- e=[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]

-- rangeAdd hm x = H.adjust (\y-> y+1) ((fromIntegral $ floor (x*10))/10 :: Double, (fromIntegral $ ceiling (x*10))/10 :: Double) hm

-- rangeAdd hm x = H.adjust (\y-> y+1) (c-0.1,c) hm
--   where
--     c = (fromIntegral $ ceiling (x*10))/10 :: Double

-- clamp mn mx val = max mn (min val mx)

-- matrixUpdate :: (a -> a) -> (Int,Int) -> V.Vector (V.Vector a) -> V.Vector (V.Vector a)
-- matrixUpdate f pos matrix = V.accum (\x y-> V.accum (\a b-> b a) x [(snd pos,y)]) matrix [(fst pos, f)]

-- threshold (r,g,b) (r1,g1,b1) = (<) 220 $ abs $ (r+g+b)-(r1+g1+b1)
-- isDarker (r,g,b) (r1,g1,b1) = (<) (div (r+g+b) 3) (div (r1+g1+b1) 3)

-- getEdgeIndices :: [[(Int,Int,(Int,Int,Int))]] -> [(Int,Int)]
-- getEdgeIndices s = concat.map (concat.map isDisc) $ s
--   where
--     (mr,mc) = (length s,length $ head s)
--     isDisc (rw,cl,p1@(r,g,b)) = map (\(x,y,p)-> if isDarker p p1 then (x,y) else (rw,cl)) $ filter (\(_,_,p2)-> threshold p1 p2 ) $ neighIndicesWP rw cl
--     neighIndicesWP rw cl = map (\(x,y) -> (s!!x)!!y) $ neighIndices rw cl
--     neighIndices rw cl = filter (\(x,y)-> x<mr && x>=0 && y<mc && y>=0) [(rw-1,cl),(rw+1,cl),(rw,cl-1),(rw,cl+1),(rw-1,cl-1),(rw+1,cl+1),(rw-1,cl+1),(rw+1,cl-1)]


-- getPts (x,y) = concat $ map (\i-> map (\j-> (x*5+i,y*5+j)) [0..4]) [0..4]
-- fun (x,y) = (G.index g ((x*5)+2,(y*5)+2))
-- g2 = G.makeImage ((\(x,y)-> (div x 5,div y 5)) (G.dims g)) fun :: GI.Image G.VU G.Y Double

-- g <- readFile "properSet"
-- g1 = read  g :: [[(Double,Double)]]
-- g2 = head $ g1
-- g31 = V.fromList $ fst (unzip g2)
-- g32 = V.fromList $ snd (unzip g2)
-- linearRegressionTLS g31 g32
