import KDTree2d
import Vec2
import System.Random
import System.IO.Unsafe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
-- import Data.Monoid.Writer
import Control.Monad.Writer.Lazy
import Data.Maybe  (fromJust, isJust)
import Data.List (minimumBy,find,delete,foldl')
import Data.Function (on)
-- for random seed
--import Data.Time (formatTime, getCurrentTime)
--import System.Locale (defaultTimeLocale)
--import Control.Applicative

data Food = Food { fposition :: Vec2} deriving (Eq)

data Boid = Boid { identifier :: Int,
                   bposition :: Vec2,
                   velocity :: Vec2,
                   hunger :: Double, -- in [0,1] if hunger reaches 1, then the Boid starves
                   cohesionScale :: Double,
                   separationScale :: Double,
                   alignmentScale :: Double,
                   dbgC :: Vec2,
                   dbgS :: Vec2,
                   dbgA :: Vec2 } deriving (Eq, Show)

data World = World { width :: Double,
                     height :: Double,
                     pixWidth :: Int,
                     pixHeight :: Int } deriving (Eq, Show)

{--
data Params = Params { aParam :: Double,
                       sScale :: Double,
                       cParam :: Double
                     }
--}
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
boidColor :: Boid -> Color
boidColor b     = makeColor 1.0 1.0 0.0 ( realToFrac (1-(hunger b)) )
-- radiusColor     = makeColor 0.5 1.0 1.0 0.2
cohesionColor :: Boid -> Color
cohesionColor b = makeColor scaled 0.0 0.0 1.0
    where scaled = realToFrac $ (cohesionScale b) / maxcohesion
separationColor :: Boid -> Color
separationColor b = makeColor 0.0 scaled 0.0 1.0
    where scaled = realToFrac $ (separationScale b) / maxseparation
alignmentColor :: Boid -> Color
alignmentColor b = makeColor 0.0 0.0 scaled 1.0
    where scaled = realToFrac $ (alignmentScale b) / maxalignment

renderboid :: World -> Boid -> Picture
renderboid world b =
  let (Vec2 x y) = bposition b
      sf = 5.0 * (scaleFactor world)
      sf' = 1.0/3.0 * (scaleFactor world)
      sf''= 2.0/3.0 * (scaleFactor world)
      sf'''=1 * (scaleFactor world)
      (xs,ys) = modelToScreen world (x,y)
  in
    Pictures $ [
      Color (boidColor b) $
        Translate xs ys $
        Circle 2 , -- a dying boid will be faded and a full boid will be bright yellow
      Color ( cohesionColor b ) $
        Translate xs ys $
        Circle ((realToFrac epsilon) * sf'),
      Color (separationColor b) $
        Translate xs ys $
        Circle ((realToFrac epsilon) * sf''),
      Color ( alignmentColor b ) $
        Translate xs ys $
        Circle ((realToFrac epsilon) * sf''')
    ]

-- TODO check type signature!
renderfoods :: World -> [Food] -> [Picture]
renderfoods world [] = []
renderfoods world foods = 
  let foi : rest = foods -- food of interest : rest of food
      (Vec2 x y) = fposition foi
      (xs,ys) = modelToScreen world (x,y)
  in -- food will appear as small white squares
      ( Color   (makeColor 1 1 1 1)
            -- $ Translate xs ys 
             $ Polygon [(xs-1,ys-1),(xs+1 ,ys-1),(xs+1 ,ys+1),(xs-1 ,ys+1),(xs-1,ys-1)]
      ) : renderfoods world rest

-- TODO check the type signature here :/
renderboids :: (RandomGen g) => World -> (KDTreeNode Boid , [Food], g) -> Picture
renderboids world (bs,fs,_gen) =
    (Pictures $ (mapKDTree bs (renderboid world))++(renderfoods world fs))

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
initialize :: Int -> Double -> Double -> IO [Boid]
initialize n sp sv = do
  nums <- rnlist (n*7)
  let makeboids [] [] = []
      makeboids (a:b:c:d:e:f:g:rest) (id:ids) =
         (Boid {identifier = id,
                velocity = Vec2 (sv*(0.5 - a)/2.0) (sv*(0.5 - b)/2.0),
                bposition = Vec2 (sp*(0.5 - c)/2.0) (sp*(0.5 - d)/2.0),
                cohesionScale = maxcohesion * e,
                separationScale = maxseparation * f,
                alignmentScale = maxalignment * g,
                hunger = 0,
                dbgC = vecZero,
                dbgS = vecZero,
                dbgA = vecZero}) : makeboids rest ids
  return $ makeboids nums [1..n]

--initializeFoods :: Int -> Double -> Double -> IO [Food]
initializeFoods numBoids scalePos = do
    nums <- (rnlist $ 2*(numBoids `div `2)) -- for 2 boids, there will be one food. Let them STARVE! muhaha
    let makefoods [] = []
        makefoods (a:b:rest) = 
            Food {fposition = Vec2 (scalePos*(0.5-a)/2.0) (scalePos*(0.5-b)/2.0)} :
                               makefoods rest
    return (makefoods nums)
{--

==========================================================================
VECTOR HELPERS
==========================================================================

--}

-- sometimes we want to control runaway of vector scales, so this can
-- be used to enforce an upper bound
limiter :: Vec2 -> Double -> Vec2
limiter x lim = let d = vecNorm x
                in if (d < lim)
                    then x
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
--
world = World { width = (maxx-minx), height = (maxy-miny), pixWidth = 700, pixHeight = 700 }

maxcohesion :: Double
maxcohesion = 0.0175 -- originally 0.0075

sParam = 1.25 -- originally 1.25
maxseparation = 0.2 -- orignally 0.1

sight = 3*(scaleFactor world) -- :: Double
hungerScale = 2.0  -- :: Double

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

-- food seeking behaviour
euclidean :: Vec2 -> Vec2 -> Double
euclidean (Vec2 x1 y1) (Vec2 x2 y2) = sqrt ( (x1-x2)^2 + (y1-y2)^2 )

seekFood :: Boid -> [Food] -> Vec2
seekFood b foods = 
    let p = bposition b
        foodpos = map (fposition) foods
        goToClosestFood = 
         case foods of 
         [] -> vecZero
         _  -> vecSub (minimumBy (compare `on` (euclidean p)) foodpos) p
    in vecScale (vecScale goToClosestFood (hunger b)) hungerScale

-- three rules : cohesion (seek centroid), separation (avoid neighbors),
-- and alignment (fly same way as neighbors)
-- centroid is average position of boids, or the vector sum of all
--
-- boid positions scaled by 1/(number of boids)
--
findCentroid :: [Boid] -> Vec2
findCentroid []    = error "Bad centroid"
findCentroid boids =
  let n = length boids
  in vecScale (foldl1 vecAdd (map bposition boids)) (1.0 / (fromIntegral n))

--
-- cohesion : go towards centroid.  parameter dictates fraction of
-- distance from boid to centroid that contributes to velocity
--
cohesion :: Boid -> [Boid] -> Double -> Vec2
cohesion b boids a = vecScale diff a
  where c = findCentroid boids
        p = bposition b
        diff = vecSub c p

-- separation.
separation :: Boid -> [Boid] -> Double -> Vec2
separation b []    a = vecZero
separation b boids a =
  let diff_positions = map (\i -> vecSub (bposition i) (bposition b)) boids
      closeby = filter (\i -> (vecNorm i) < a) diff_positions
      sep = foldl vecSub vecZero closeby
  in
    vecScale sep sParam

-- alignment
alignment :: Boid -> [Boid] -> Double -> Vec2
alignment b [] a = vecZero
alignment b boids a =
  let v = foldl1 vecAdd (map velocity boids)
      s = 1.0 / (fromIntegral $ length boids)
      v' = vecScale v s
  in
   vecScale (vecSub v' (velocity b)) a

distance :: Vec2 -> Vec2 -> Double
distance v1 v2 = sqrt((x1-x2)^2+(y1-y2)^2)
    where (Vec2 x1 y1)=v1
          (Vec2 x2 y2)=v2

-- one boid
oneboid :: Boid -> [Boid] -> [Food] -> (Boid,[Food])
oneboid b boids foods=
  let c = cohesion b boids (cohesionScale b) `vecScale` 0.1
      s = separation b boids (separationScale b) `vecScale` 0.1
      a = alignment b boids (alignmentScale b) `vecScale` 0.1
      f = seekFood b foods
      p = bposition b
      v = velocity b
      v' = foldl' vecAdd (Vec2 0 0) [ v, c, s, a, (edge_repel p), f ]
      v'' = limiter (vecScale v' 1.0025) vLimit
      p' = vecAdd p v''
      maybefood = find ( \foodParticle -> (distance p (fposition foodParticle)) <= epsilon )
                  foods
         -- maybefood is food that it can eat this round
      (remainingFoods,newhunger)=
        case  maybefood of
         (Just food) -> (delete food foods, 0)
         Nothing ->     (foods , (hunger b) + 0.01)
  in
      (b { bposition = wraparound p',
                   velocity = v'',
                   hunger = newhunger,
                   dbgC = c,
                   dbgS = s,
                   dbgA = a},
               remainingFoods)

-- Fear edges
-- Pos -> Delta Velocity
edge_repel :: Vec2 -> Vec2
edge_repel (Vec2 x y) = Vec2 (repel x maxx minx) (repel y maxy miny)

      -- Pos    -> Bound  -> Bound  -> Accel
repel :: Double -> Double -> Double -> Double
repel x maxx minx | (x - minx) < cap =   c / (x - minx)**2
                  | (maxx - x) < cap =  -c / (maxx - x)**2
                  | otherwise = 0
    where c = 0.001
          cap = 2

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
  let p = bposition b

      -- bounds
      vlo = vecSub p epsvec
      vhi = vecAdd p epsvec

      -- split the boxes
      splith = splitBoxHoriz (vlo, vhi, 0.0, 0.0)
      splitv = concatMap splitBoxVert splith

      -- adjuster for wraparound
      adj1 ax ay (pos, theboid) = (vecAdd pos av,
                                   theboid { bposition = vecAdd p av })
        where av = Vec2 ax ay
              p = bposition theboid

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

-- there is a 1 in 100 chance that a new pice of food will appear on this iteration
-- number of boids -> list of foods
growFood :: RandomGen g => Int -> Double -> g -> ([Food],g)
growFood n sp randGen = (foods,finalRanGen)
    where
    (i,randGen')  = randomR (0,100) randGen
    (foods,finalRanGen)= if i==(1::Int) 
                         then ([],randGen') 
                         else
                          let 
                           (a,randGen'') = randomR (0,1) randGen'
                           (b,randGen''')= randomR (0,1) randGen''
                          in
                           ([Food {fposition=Vec2(sp*(0.5-a)/2.0) (sp*(0.5-b)/2.0)}],
                            randGen''')
       

-- writer traverse tree. modifed from mapKDTree
-- wtraverseTree :: KDTreeNode a -> [Food] ->  (Writer [Food] a -> Writer [Food] b) -> ([Food],[b])
{-
wtraverseTree Empty _ _ = ([],[]) -- Check this
wtraverseTree (Node Empty pos boid right) foods f = 
  (removedBoidList,foodlist)foods
  where
    (updatedboid,remainingFoods) = runWriter $ f ( writer(boid,foods) )
    (foodlist,boidlist)          = wtraverseTree right remainingFoods f
    removedBoidList              =if (hunger updatedboid) > 0 
                                 then updatedboid:boidlist else boidlist
wtraverseTree (Node (Node leftl positionl boidl rightl) pos boid right) foods f =
    wtraverseTree (Node leftl positionl boidl (Node rightl pos boid right)) foods f
-}
-- :: [State [Food] Boid] -> State [Food] [Boids]
serialize ::  [([Food] -> (Boid, [Food]))] -> [Food] -> ([Boid], [Food])
serialize [] foods = ([], foods)
serialize (a:as) foods = let (b, newFoods) = a foods
                             (bs, finalFood) = serialize as newFoods in
    (b:bs, finalFood)

-- iterationkd :: Int -> Double -> ViewPort -> Float -> (KDTreeNode Boid,[Food]) -> (KDTreeNode Boid, [Food])
iterationkd n sp step (w,foods,randGen) = 
  let 
    actions = map (\b -> oneboid b (findNeighbors w b)) $ kdtreeToList w
    (boids,rfoods) = serialize actions foods
    (addedFood,randGen') = growFood n sp randGen 
  in (foldl (\t b -> kdtAddPoint t (bposition b) b) newKDTree boids, addedFood++rfoods, randGen')

main :: IO ()
main = 
  let sp = 10.0
      sv = 0.5
      n  = 5 in do
  randGen3 <- getStdGen
  (bs) <- initialize n sp sv -- boids starting
  (fs) <- initializeFoods n sp -- foods starting
    -- tf is (tree,food)
  let
    tf = (foldl (\t b -> kdtAddPoint t (bposition b) b) newKDTree bs, -- newKDTree
          fs, randGen3)
  play
    (InWindow "Boids" (pixWidth world, pixHeight world) (10,10))
    (greyN 0.1)
    30
    tf
    (renderboids world)
    (\_ w -> w)
    (iterationkd n sp)-- TODO what is happening here?? what?? is there side effects????
