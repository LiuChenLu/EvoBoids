import KDTree2d
import Vec2
import System.Random
import System.IO.Unsafe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Simulate
-- for random seed
--import Data.Time (formatTime, getCurrentTime)
--import System.Locale (defaultTimeLocale)
--import Control.Applicative 

data Boid = Boid { identifier :: Int,
                   position :: Vec2,
                   velocity :: Vec2, 
                   cohesionScale :: Int,
                   separationScale :: Int,
                   alignmentScale :: Int,
                   dbgC :: Vec2,
                   dbgS :: Vec2,
                   dbgA :: Vec2 } deriving Show

data World = World { width :: Double,
                     height :: Double,
                     pixWidth :: Int,
                     pixHeight :: Int } deriving Show

data Params = Params { aParam :: Int,
                       sParam :: Int,
                       sScale :: Int,
                       cParam :: Int
                     }

{--

==========================================================================
GRAPHICS 
==========================================================================

--}

modelToScreen :: World -> (Double, Double) -> (Float, Float)
modelToScreen world (x,y) =
  let xscale = (fromIntegral (pixWidth world)) / (width world)
      yscale = (fromIntegral (pixHeight world)) / (height world)
  in
    (realToFrac $ x * xscale, realToFrac $ y * yscale)

scaleFactor :: World -> Float
scaleFactor world =
  let xscale = (fromIntegral (pixWidth world)) / (width world)
      yscale = (fromIntegral (pixHeight world)) / (height world)
  in
   realToFrac $ max xscale yscale

velocityScale :: Float
velocityScale = 10.0 * (realToFrac (max (maxx-minx) (maxy-miny)) :: Float)

--
-- colors
--
boidColor       = makeColor 1.0 1.0 0.0 1.0
-- radiusColor     = makeColor 0.5 1.0 1.0 0.2
cohesionColor :: Boid -> Color
cohesionColor b = makeColor scaled 0.0 0.0 1.0
    where scaled = (cohesionScale b) / maxcohesion
separationColor :: Boid -> Color
separationColor = makeColor 0.0 scaled 0.0 1.0
    where scaled = (separationScale b) / maxseparation
alignmentColor :: Boid -> Color
alignmentColor  = makeColor 0.0 0.0 scaled 1.0
    where scaled = (alignmentScale b) / maxalignment

renderboid :: World -> Boid -> Picture
renderboid world b =
  let (Vec2 x y) = position b
      --(Vec2 vx vy) = velocity b
      --v = velocity b
--      (Vec2 dCX dCY) = dbgC b
--       (Vec2 dSX dSY) = dbgS b
--       (Vec2 dAX dAY) = dbgA b
      sf = 5.0 * (scaleFactor world)
      sf' = 0.1 * (scaleFactor world)
      sf''= 0.2 * (scaleFactor world)
--      sf'''= 0.3 * (scaleFactor world)
      --sf2 = sf * 10
      (xs,ys) = modelToScreen world (x,y)
--       vxs = sf * (realToFrac vx) :: Float
--       vys = sf * (realToFrac vy) :: Float
  in
    Pictures $ [
      Color boidColor $ 
        Translate xs ys $
        Circle 2 ,
      Color ( cohesionColor b ) $
        Translate xs ys $
        Circle ((realToFrac epsilon) * sf'),
      Color (separationColor b) $
        Translate xs ys $
        Circle ((realToFrac epsilon) * sf''),
      Color ( alignmentColor b ) $
        Translate xs ys $
        Circle ((realToFrac epsilon) * sf''')
--      Color boidColor $          
--         Line [(xs,ys), ((xs+vxs),(ys+vys))],
--       Color cohesionColor $
--         Line [(xs,ys), ((xs+(sf2*realToFrac dCX)),(ys+(sf2*realToFrac dCY)))],
--       Color alignmentColor $
--         Line [(xs,ys), ((xs+(sf2*realToFrac dAX)),(ys+(sf2*realToFrac dAY)))],
--       Color separationColor $
--         Line [(xs,ys), ((xs+(sf'*realToFrac dSX)),(ys+(sf'*realToFrac dSY)))]
    ]

renderboids :: World -> KDTreeNode Boid -> IO Picture
renderboids world bs = do return (Pictures $ mapKDTree bs (renderboid world))

{--

==========================================================================
INITIALIZATION
==========================================================================

--}

rnlist :: Int -> IO [Double]
rnlist n = do
  mapM (\_ -> randomRIO (0,1)) [1..n]

-- sp is scale position
-- sv is scale velocity
initialize :: Int -> Double -> Double -> [Boid]
initialize n sp sv =
  let generator = mkStdGen $ 
        (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int
      nums = unsafePerformIO $ rnlist (n*7) 
      -- nums' = map (\i -> (0.5 - i)/2.0) nums
      makeboids [] [] = []
      makeboids (a:b:c:d:e:f:g:rest) (id:ids) = 
         (Boid {identifier = id,
                velocity = Vec2 (sv*(0.5 - a)/2.0) (sv*(0.5 - b)/2.0),
                position = Vec2 (sp*(0.5 - c)/2.0) (sp*(0.5 - d)/2.0),
                cohesionScale = maxcohesion * e
                separationScale = maxseparation * f
                alignmentScale = maxalignment * g
                dbgC = vecZero,
                dbgS = vecZero,
                dbgA = vecZero}) : makeboids rest ids
  in
    makeboids nums [1..n]

{--

==========================================================================
VECTOR HELPERS
==========================================================================

--}

-- sometimes we want to control runaway of vector scales, so this can
-- be used to enforce an upper bound
limiter :: Vec2 -> Double -> Vec2
limiter x lim = let d = vecNorm x
                in if (d < lim) then x
	               else vecScale (vecNormalize x) lim

--
-- vector with all components length epsilon
--
epsvec :: Vec2
epsvec = Vec2 epsilon epsilon


{--

==========================================================================
PARAMETERS
==========================================================================

--}

maxcohesion = 0.0175 -- originally 0.0075

--sParam = (max (maxx-minx) (maxy-miny)) / 10.0
sParam = 1 -- originally 0.1
maxseparation = 2 -- orignally 1.25 

maxalignment = 1.5 -- orignally 1.0 / 1.8
vLimit = 0.0025 * (max (maxx-minx) (maxy-miny))
epsilon = 0.40
maxx = 8.0
maxy = 8.0
minx = -8.0
miny = -8.0


{--

==========================================================================
BOIDS LOGIC
==========================================================================

--}

-- three rules : cohesion (seek centroid), separation (avoid neighbors),
-- and alignment (fly same way as neighbors)

--
-- centroid is average position of boids, or the vector sum of all
-- boid positions scaled by 1/(number of boids)
--
findCentroid :: [Boid] -> Vec2
findCentroid []    = error "Bad centroid"
findCentroid boids =
  let n = length boids
  in vecScale (foldl1 vecAdd (map position boids)) (1.0 / (fromIntegral n))

--
-- cohesion : go towards centroid.  parameter dictates fraction of
-- distance from boid to centroid that contributes to velocity
--
cohesion :: Boid -> [Boid] -> Double -> Vec2
cohesion b boids a = vecScale diff a
  where c = findCentroid boids
        p = position b
        diff = vecSub c p
        
-- separation.  
separation :: Boid -> [Boid] -> Double -> Vec2
separation b []    a = vecZero
separation b boids a =
  let diff_positions = map (\i -> vecSub (position i) (position b)) boids
      closeby = filter (\i -> (vecNorm i) < a) diff_positions
      sep = foldl vecSub vecZero closeby
  in
    vecScale sep sScale

-- alignment
alignment :: Boid -> [Boid] -> Double -> Vec2
alignment b [] a = vecZero
alignment b boids a =
  let v = foldl1 vecAdd (map velocity boids)
      s = 1.0 / (fromIntegral $ length boids)
      v' = vecScale v s
  in
   vecScale (vecSub v' (velocity b)) a

-- one boid
oneboid :: Boid -> [Boid] -> Boid
oneboid b boids =
  let c = cohesion b boids (cohesionScale b)
      s = separation b boids (separationScale b)
      a = alignment b boids (alignmentScale b)
      p = position b
      v = velocity b
      id = identifier b
      v' = vecAdd v (vecScale (vecAdd c (vecAdd s a)) 0.1)
      v'' = limiter (vecScale v' 1.0025) vLimit
      p' = vecAdd p v''
  in
   b { position = wraparound p',
       velocity = v'', 
       dbgC = c,
       dbgS = s,
       dbgA = a}

--
-- Neighbor finding code
--
-- This is slightly tricky if we want to represent a world that wraps
-- around in one or more dimensions (aka, a torus or cylinder).
--
-- The issue is that we need to split the bounding box that we query the
-- KDTree with when that box extends outside the bounds of the world.
-- Furthermore, when a set of boids are found in the split bounding boxes
-- representing a neighbor after wrapping around, we need to adjust the
-- relative position of those boids with respect to the reference frame
-- of the central boid.  For example, if the central boid is hugging the left
-- boundary, and another boid is right next to it hugging the right
-- boundary, their proper distance is likely very small.  If the one on the
-- right boundary isn't adjusted, then the distance will actually appear to
-- be very large (approx. the width of the world).
--

findNeighbors :: KDTreeNode Boid -> Boid -> [Boid]
findNeighbors w b =
  let p = position b
      
      -- bounds
      vlo = vecSub p epsvec
      vhi = vecAdd p epsvec
      
      -- split the boxes
      splith = splitBoxHoriz (vlo, vhi, 0.0, 0.0)
      splitv = concatMap splitBoxVert splith
      
      -- adjuster for wraparound
      adj1 ax ay (pos, theboid) = (vecAdd pos av,
                                   theboid { position = vecAdd p av })
        where av = Vec2 ax ay
              p = position theboid

      adjuster lo hi ax ay = let neighbors = kdtRangeSearch w lo hi
                             in map (adj1 ax ay) neighbors
      
      -- do the sequence of range searches
      ns = concatMap (\(lo,hi,ax,ay) -> adjuster lo hi ax ay) splitv
      
      -- compute the distances from boid b to members
      dists = map (\(np,n) -> (vecNorm (vecSub p np), n)) ns
  in
    b:(map snd (filter (\(d,_) -> d<=epsilon) dists))


splitBoxHoriz :: (Vec2,Vec2,Double,Double) -> [(Vec2,Vec2,Double,Double)]
splitBoxHoriz (lo@(Vec2 lx ly), hi@(Vec2 hx hy), ax, ay) =
  if (hx-lx > w) 
  then [(Vec2 minx ly, Vec2 maxx hy, ax, ay)]
  else if (lx < minx) 
       then [(Vec2 minx ly, Vec2 hx hy, ax, ay),
             (Vec2 (maxx-(minx-lx)) ly, Vec2 maxx hy, (ax-w), ay)]
       else if (hx > maxx) 
            then [(Vec2 lx ly, Vec2 maxx hy, ax, ay),
                  (Vec2 minx ly, Vec2 (minx + (hx-maxx)) hy, ax+w, ay)]
            else [(lo,hi,ax,ay)]
  where w = maxx-minx

splitBoxVert :: (Vec2,Vec2,Double,Double) -> [(Vec2,Vec2,Double,Double)]
splitBoxVert (lo@(Vec2 lx ly), hi@(Vec2 hx hy), ax, ay) =
  if (hy-ly > h) 
  then [(Vec2 lx miny, Vec2 hx maxy, ax, ay)]
  else if (ly < miny) 
       then [(Vec2 lx miny, Vec2 hx hy, ax, ay),
             (Vec2 lx (maxy-(miny-ly)), Vec2 hx maxy, ax, ay-h)]
       else if (hy > maxy) 
            then [(Vec2 lx ly, Vec2 hx maxy, ax, ay),
                  (Vec2 lx miny, Vec2 hx (miny + (hy-maxy)), ax, ay+h)]
            else [(lo,hi,ax,ay)]
  where h = maxy-miny

wraparound :: Vec2 -> Vec2
wraparound (Vec2 x y) =
 let w = maxx-minx
     h = maxy-miny
     x' = if (x>maxx) then x-w else (if x<minx then x+w else x)
     y' = if (y>maxy) then y-h else (if y<miny then y+h else y)
 in Vec2 x' y'

iterationkd :: ViewPort -> Float -> KDTreeNode Boid -> IO (KDTreeNode Boid)
iterationkd vp step w = do
  let boids = mapKDTree w (\i -> oneboid i (findNeighbors w i))
  return $ foldl (\t b -> kdtAddPoint t (position b) b) newKDTree boids
    
iteration :: ViewPort -> Float -> KDTreeNode Boid -> KDTreeNode Boid
iteration vp step w =
  let all = kdtreeToList w
      boids = mapKDTree w (\i -> oneboid i all)
  in
    foldl (\t b -> kdtAddPoint t (position b) b) newKDTree boids

main :: IO ()
main = do
  let w = World { width = (maxx-minx), height = (maxy-miny), pixWidth = 700, pixHeight = 700 }
      bs = initialize 1500 10.0 0.5
      t = foldl (\t b -> kdtAddPoint t (position b) b) newKDTree bs
  simulateIO
    (InWindow "Boids" (pixWidth w, pixHeight w) (10,10))
    (greyN 0.1)
    30 
    t
    (renderboids w)
    iterationkd
