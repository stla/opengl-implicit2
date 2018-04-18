module MarchingCubes.MarchingCubes
  (XYZ, Triangle, Voxel, marchingCubes, makeVoxel, computeContour3d')
  where
import           Data.Array.Unboxed     (UArray, amap, array, bounds, elems,
                                         (!))
import           Data.List              (findIndices, transpose)
import           Data.List.Split        (chunksOf)
import           Data.Maybe             (fromMaybe)
import           Data.Tuple.Extra       (fst3, snd3, swap, thd3)
import qualified Data.Vector            as V
import           Data.Vector.Unboxed    (Vector)
import qualified Data.Vector.Unboxed    as UV
import           MarchingCubes.Matrices
import           MarchingCubes.Tables
import           MarchingCubes.Utils    (toTriplet)

type XYZ = (Double,Double,Double)
type Triangle = ((Double,Double,Double),(Double,Double,Double),(Double,Double,Double))

toTriangle :: [[Double]] -> Triangle
toTriangle triangleAsList = toTriplet (map toTriplet triangleAsList)

toTriangles :: [[Double]] -> [Triangle]
toTriangles trianglesAsList = map toTriangle (chunksOf 3 trianglesAsList)

faceType :: UArray (Int,Int) Double -> Int -> Int -> Double -> Double -> UArray (Int,Int) Int
faceType v nx ny level maxvol = foldr matricialSum v1 [v2,v3,v4]
  where
  comparison = if level == maxvol then fromEnum . (>= level) else fromEnum . (> level)
  v0 = amap comparison v
  v1 = minorMatrix v0 nx ny
  v2 = scaledMatrix 2 (minorMatrix v0 1 ny)
  v3 = scaledMatrix 4 (minorMatrix v0 1 1)
  v4 = scaledMatrix 8 (minorMatrix v0 nx 1)

levCells :: UArray (Int,Int,Int) Double -> Double -> Double
         -> ((Vector Int,Vector Int,Vector Int), Vector Int)
levCells a level maxvol =
  ((v_i, v_j, v_k), UV.fromList $ concatMap snd cellsAndTypes)
  where
  ((_,_,_),(nx,ny,nz)) = bounds a
  types = V.fromList $ map (\k -> faceType (toMatrix a k) nx ny level maxvol) [1 .. nz]
  f k = (map (+ (nx-1)*(ny-1)*(k-1)) contourCells,
         [cellTypes ! swap ij | ij <- snd intind])
    where
    cellTypes = matricialSum (types V.! (k-1)) (scaledMatrix 16 (types V.! k))
    intind = findIndicesAsIntegers (\x -> x>0 && x<255) cellTypes
    contourCells = fst intind
  cellsAndTypes = map f [1 .. (nz-1)]
  cells = concatMap fst cellsAndTypes
  v_i = UV.fromList $ map ((+1) . (`mod` (nx-1))) cells
  v_j = UV.fromList $ map ((+1) . (`mod` (ny-1)) . (`div` (nx-1))) cells
  v_k = UV.fromList $ map ((+1) . (`div` ((nx-1)*(ny-1)))) cells

getBasic :: [Int] -> UArray (Int,Int,Int) Double -> Double
         -> ((Vector Int,Vector Int,Vector Int), Vector Int)
         -> ((Vector Double, (Vector Double,Vector Double,Vector Double)), [Int], [Int])
getBasic r vol level ((v_i,v_j,v_k),v_t) =
  ((values, (info1, info2, info3)), p1, cases)
  where
  v_i' = [v_i UV.! (i-1) | i <- r]
  v_j' = [v_j UV.! (i-1) | i <- r]
  v_k' = [v_k UV.! (i-1) | i <- r]
  cube_1 = transpose [v_i',v_j',v_k']
  index :: [[Int]]
  index = [ [0, 0, 0],
            [1, 0, 0],
            [1, 1, 0],
            [0, 1, 0],
            [0, 0, 1],
            [1, 0, 1],
            [1, 1, 1],
            [0, 1, 1] ]
  k1 = concat $ replicate (length r) index
  k2 = concatMap (replicate 8) cube_1
  cube_co = zipWith (zipWith (+)) k1 k2
  values = UV.fromList $
            map (subtract level) [vol ! toTriplet ijk | ijk <- cube_co] ++ [0]
  fromInt :: Int -> Double
  fromInt = fromIntegral
  information_matrix = transpose (map (map fromInt) (cube_co ++ [[0,0,0]]))
  info1 = UV.fromList (information_matrix !! 0)
  info2 = UV.fromList (information_matrix !! 1)
  info3 = UV.fromList (information_matrix !! 2)
  p1 = map ((+1) . (*8)) [0 .. length r -1]
  cases = [v_t UV.! (i-1) | i <- r]

edges_p1rep_1 :: [Int] -> [Int] -> ([Int],[Int])
edges_p1rep_1 cases p1 =
  (UV.toList $ UV.concat edges, concatMap (uncurry replicate) (zip counts p1))
  where
  edges = map V.head [edgesTableV V.! (i-1) | i <- cases]
  counts = map UV.length edges

getPoints :: [Int] -> [Int] -> (Vector Double, (Vector Double,Vector Double,Vector Double))
          -> [[Double]]
getPoints edges p1 (values, (info1, info2, info3)) = out
  where
  x1 = [edgePoints1 UV.! (i-1) | i <- edges]
  x2 = [edgePoints2 UV.! (i-1) | i <- edges]
  floor' :: Double -> Int
  floor' = floor
  lambda = map (realToFrac . floor' . (/9) . fromIntegral) x1
  mu = map (\x -> 1-x) lambda
  average w w' = zipWith (+) (zipWith (*) mu w) (zipWith (*) lambda w')
  average7 w = zipWith (+) (zipWith (*) mu w) lambda
  average8 w = zipWith (-) (zipWith (*) mu w) lambda
  p1x1 = zipWith (+) p1 x1
  p1x2 = zipWith (+) p1 x2
  v1  = [info1 UV.! (i-2) | i <- p1x1]
  v1' = [info1 UV.! (i-1) | i <- p1]
  v2  = [info1 UV.! (i-2) | i <- p1x2]
  v2' = [info1 UV.! i | i <- p1]
  v3  = [info2 UV.! (i-2) | i <- p1x1]
  v3' = [info2 UV.! i | i <- p1]
  v4  = [info2 UV.! (i-2) | i <- p1x2]
  v4' = [info2 UV.! (i+1) | i <- p1]
  v5  = [info3 UV.! (i-2) | i <- p1x1]
  v5' = [info3 UV.! i | i <- p1]
  v6  = [info3 UV.! (i-2) | i <- p1x2]
  v6' = [info3 UV.! (i+4) | i <- p1]
  v7  = [values UV.! (i-2) | i <- p1x1]
  v8  = [values UV.! (i-2) | i <- p1x2]
  out = [ average v1 v1'
        , average v2 v2'
        , average v3 v3'
        , average v4 v4'
        , average v5 v5'
        , average v6 v6'
        , average7 v7
        , average8 v8
        ]

calPoint :: [[Double]] -> [[Double]]
calPoint info =
  [scale (info!!0) (info!!1), scale (info!!2) (info!!3), scale (info!!4) (info!!5)]
  where
  s = zipWith (/) (info!!6) (zipWith (-) (info!!6) (info!!7))
  scale u v = zipWith (+) u (zipWith (*) s (zipWith (-) v u))

preRender1 :: [Int] -> [Int] -> (Vector Double, (Vector Double,Vector Double,Vector Double))
           -> [[Double]]
preRender1 cases p1 information = transpose $ calPoint info
  where
  (edges, p1rep) = edges_p1rep_1 cases p1
  info = getPoints edges p1rep information

getTriangles1 :: [Int] -> UArray (Int,Int,Int) Double -> Double
              -> ((Vector Int,Vector Int,Vector Int), Vector Int) -> [[Double]]
getTriangles1 r vol level v = preRender1 cases p1 information
  where
  basics = getBasic r vol level v
  information = fst3 basics
  p1 = snd3 basics
  cases = thd3 basics

computeContour3d :: UArray (Int,Int,Int) Double -> Maybe Double -> Double
                 -> [Triangle]
computeContour3d vol maxvol' level =
  toTriangles triangles
  where
  maxvol = fromMaybe (maximum (elems vol)) maxvol'
  v = levCells vol level maxvol
  tcase = [caseRotationFlip0 UV.! i | i <- UV.toList $ snd v]
  r = map (+1) $ findIndices (`elem` [2, 3, 6, 9, 10, 12, 15]) tcase
  triangles = if not $ null r then getTriangles1 r vol level v else [[]]


marchingCubes :: ((Double,Double,Double) -> Double)   -- function
              -> Double            -- isolevel
              -> ((Double,Double),(Double,Double),(Double,Double))  -- bounds
              -> Int               -- grid subdivisions
              -> [Triangle]
marchingCubes fun level xyzbounds subd = map (rescale xyzbounds) triangles
  where
  triangles = computeContour3d (fun2array subd xyzbounds fun) Nothing level
  fun2array n ((xm,xM),(ym,yM),(zm,zM)) f =
    array ((1,1,1),(n,n,n))
          [((i,j,k), f (x,y,z)) | i <- [1..n], j <- [1..n], k <- [1..n],
                                  let x = sx i,
                                  let y = sy j,
                                  let z = sz k]
    where
    s a b l = a + (b-a) * fromIntegral (l-1) / fromIntegral (n-1)
    sx = s xm xM
    sy = s ym yM
    sz = s zm zM
  rescale ((xm,xM),(ym,yM),(zm,zM)) ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) =
    ((sx' x1, sy' y1, sz' z1), (sx' x2, sy' y2, sz' z2), (sx' x3, sy' y3, sz' z3))
    where
    s' a b u = a + (b-a) * u / fromIntegral (subd-1)
    sx' = s' xm xM
    sy' = s' ym yM
    sz' = s' zm zM

-- -- -- -- -- -- --
type Voxel = (UArray (Int,Int,Int) Double,
              ((Double,Double),(Double,Double),(Double,Double)),
              (Int,Int,Int))

makeVoxel :: ((Double,Double,Double) -> Double)
          -> ((Double,Double), (Double,Double), (Double,Double))
          -> (Int, Int, Int) -> Voxel
makeVoxel f ((xm,xM),(ym,yM),(zm,zM)) (nx,ny,nz) =
  ( array ((1,1,1),(nx,ny,nz))
          [((i,j,k), f (x,y,z)) | i <- [1..nx], j <- [1..ny], k <- [1..nz],
                                  let x = sx i,
                                  let y = sy j,
                                  let z = sz k]
  , ((xm,xM),(ym,yM),(zm,zM))
  , (nx,ny,nz) )
  where
  s a b n l = a + (b-a) * fromIntegral (l-1) / fromIntegral (n-1)
  sx = s xm xM nx
  sy = s ym yM ny
  sz = s zm zM nz

computeContour3d' :: Voxel
                  -> Maybe Double
                  -> Double
                  -> [Triangle]
computeContour3d' (voxel, ((xm,xM),(ym,yM),(zm,zM)), (nx,ny,nz)) voxmax level =
  map rescale (toTriangles triangles)
  where
  maxvox = fromMaybe (maximum (elems voxel)) voxmax
  v = levCells voxel level maxvox
  tcase = [caseRotationFlip0 UV.! i | i <- UV.toList $ snd v]
  r = map (+1) $ findIndices (`elem` [2, 3, 6, 9, 10, 12, 15]) tcase
  triangles = if not $ null r then getTriangles1 r voxel level v else [[]]
  rescale ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) =
    ((sx x1, sy y1, sz z1), (sx x2, sy y2, sz z2), (sx x3, sy y3, sz z3))
    where
    s a b n u = a + (b-a) * u / fromIntegral (n-1)
    sx = s xm xM nx
    sy = s ym yM ny
    sz = s zm zM nz
