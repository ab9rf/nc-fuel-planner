module FuelPlanner
  (
    Fissile(..),
    Fuel(..),
    fissiles,
    fuels
  )
where

import Data.Map.Strict (Map, alter, empty, singleton, fromList,
                        findWithDefault)
import qualified Data.Map.Strict as M

import Data.Map.Merge.Strict (merge, preserveMissing, mapMissing,
                              zipWithMatched)
import Data.Maybe (fromMaybe, catMaybes)

import Numeric.LinearProgramming

data Fissile = Fissile 
             { fissileName :: String
             , fissileRtg :: Double
             , fissileDecaysTo :: Maybe Fissile
             }
  deriving (Show, Eq, Ord)
           
data Fuel = Fuel
          { fuelName :: String
          , fuelBurnDuration :: Double
          , fuelBasePower :: Double
          , fuelBaseHeat :: Double
          , fuelComponents :: Map Fissile Int
          , fuelReprocess :: Map Fissile Int
          }
  deriving (Show, Eq, Ord)

data Step = Burn Fuel
          | Decay Fissile
  deriving (Show, Eq, Ord)

(fissiles, fuels) = let 
    th232 = Fissile "Th-232" 0 Nothing
    u233 = Fissile "U-233" 0 Nothing 
    u235 = Fissile "U-235" 0 Nothing
    u238 = Fissile "U-238" ( 4.0 / 81.0 ) Nothing
    np236 = Fissile "Np-236" 0 (Just th232)
    np237 = Fissile "Np-237" 0 (Just u233)
    pu238 = Fissile "Pu-238" ( 100.0 / 9.0 ) Nothing
    pu239 = Fissile "Pu-239" 0 (Just u235)
    pu241 = Fissile "Pu-241" 0 (Just np237)
    pu242 = Fissile "Pu-242" 0 (Just u238)
    am241 = Fissile "Am-241" ( 50.0 / 9.0 ) (Just np237)
    am242 = Fissile "Am-242" 0 Nothing
    am243 = Fissile "Am-243" 0 (Just pu239)
    cm243 = Fissile "Cm-243" 0 (Just pu239)
    cm245 = Fissile "Cm-245" 0 (Just pu241)
    cm246 = Fissile "Cm-246" 0 (Just pu242)
    cm247 = Fissile "Cm-247" 0 (Just am243)
    bk247 = Fissile "Bk-247" 0 (Just am243)
    bk248 = Fissile "Bk-248" 0 (Just th232)
    cf249 = Fissile "Cf-249" 0 (Just cm245)
    cf250 = Fissile "Cf-250" ( 400.0 / 9.0 ) Nothing
    cf251 = Fissile "Cf-251" 0 (Just cm247)
    cf252 = Fissile "Cf-252" 0 (Just th232)

    fissiles =    
      [ th232 -- th230 not included because it doesn't participate in any reactions
      , u233, u235, u238
      , np236, np237
      , pu238, pu239, pu241, pu242
      , am241, am242, am243
      , cm243, cm245, cm246, cm247
      , bk247, bk248
      , cf249, cf250, cf251, cf252 
      ] 

    loweff fi fe = [ (9, fi), (72, fe) ]
    higheff fi fe = [ (36, fi), (45, fe) ]
    
    oxide (Fuel n t p h fm rp) = Fuel (n ++ " Oxide") t (p * 1.4) (h * 1.25) fm rp
    
    fuel n t p h fml rpl = Fuel n t p h (coalesce fml) (coalesce rpl)
    
    -- TODO: this could probably be written as a foldr
    coalesce :: (Ord a) => [(Int, a)] -> Map a Int
    coalesce = foldr cc empty 
      where
        upd :: Int -> Maybe Int -> Maybe Int
        upd n x = Just ((fromMaybe 0 x) + n)
        cc :: (Ord a) => (Int, a) -> Map a Int -> Map a Int
        cc (n,k) m = alter (upd n) k m

    -- the two mixed oxide fuels
    mox = 
      [ fuel "MOX-239" 80 155.4 57.5 (loweff pu239 u238) 
        [ (40, u238), (12, pu242), (8, am243), (4, cm243) ]
      , fuel "MOX-241" 46.6 243.6 97.5 (loweff pu241 u238)
        [ (40, u238), (8, pu241), (8, pu242), (8, cm246) ]
      ]
      
    -- non-oxide fuels
    -- TODO: check the reprocessing recipes for correctness
    fuels1 = 
      [ fuel "TBU"     120 60 18 (loweff th232 th232)
          [ (16, u233), (8, u235), (8, np236), (32, np237) ]
      , fuel "LEU-233" 53.3 144 60 (loweff u233 u238)
          [ (4, pu239), (4, pu241), (32, pu242), (24, am243) ] 
      , fuel "HEU-233" 53.3 576 360 (higheff u233 u238)
          [ (32, np236), (8, np237), (16, pu242), (24, am243) ]
      , fuel "LEU-235" 60 120 50 (loweff u235 u238)
          [ (40, u238 ), (8, np237), (8, pu239), (8, pu241) ]
      , fuel "HEU-235" 60 480 300 (higheff u235 u238)
          [ (20, u238), (16, np237), (4, pu239), (24, pu242) ]
      , fuel "LEN-236" 85 90 36 (loweff np236 np237)
          [ (4, np237), (32, pu242), (8, am242), (20, am243) ]
      , fuel "HEN-236" 85 360 216 (higheff np236 np237)
          [ (16, u238), (8, pu238), (8, pu239), (32, pu242) ]
      , fuel "LEP-239" 76.6 105 40 (loweff pu239 pu242)
          [ (8, pu239), (24, pu242), (4, cm243), (28, cm246) ]
      , fuel "HEP-239" 76.6 420 240 (higheff pu239 pu242)
          [ (8, am241), (24, am242), (8, cm245), (24, cm246) ]
      , fuel "LEP-241" 76.6 165 70 (loweff pu241 pu242)
          [ (16, u233), (8, u235), (8, np236), (32, np237)]
      , fuel "HEP-241" 76.6 660 420 (higheff pu241 pu242)
          [ (8, am241), (8, cm245), (24, cm246), (24, cm247)]
      , fuel "LEA-242" 45 192 94 (loweff am242 am243)
          [ (8, cm243), (8, cm245), (40, cm246), (8, cm247) ]
      , fuel "HEA-242" 45 768 564 (higheff am242 am243)
          [ (16, cm245), (32, cm246), (8, cm247), (8, bk247) ]
      , fuel "LECm-243" 43.3 210 112 (loweff cm243 cm246)
          [ (32, cm246), (16, bk247), (8, bk248), (8, cf249) ]
      , fuel "HECm-243" 43.3 840 672 (higheff cm243 cm246)
          [ (24, cm246), (24, bk247), (8, bk248), (8, cf249) ]
      , fuel "LECm-245" 56.6 162 68 (loweff cm245 cm246)
          [ (40, bk247), (8, bk248), (4, cf249), (12, cf252) ]
      , fuel "HECm-245" 56.6 648 408 (higheff cm245 cm246)
          [ (48, bk247), (4, bk248), (4, cf249), (8, cf251) ]
      , fuel "LECm-247" 65 138 54 (loweff cm247 cm246)
          [ (20, bk247), (4, bk248), (8, cf251), (32, cf252) ]
      , fuel "HECm-247" 65 552 324 (higheff cm247 cm246)
          [ (8, bk248), (8, cf249), (24, cf251), (24, cf252) ]
      , fuel "LEB-248" 71.7 135 52 (loweff bk248 bk247)
          [ (4, cf249), (4, cf251), (28, cf252), (28, cf252) ]
      , fuel "HEB-248" 71.7 540 312 (higheff bk248 bk247)
          [ (8, cf250), (8, cf251), (24, cf252), (24, cf252) ]
      , fuel "LECf-249" 50 216 116 (loweff cf249 cf252)
          [ (16, cf250), (8, cf251), (20, cf252), (20, cf252) ]
      , fuel "HECf-249" 50 864 696 (higheff cf249 cf252)
          [ (32, cf250), (16, cf251), (8, cf252), (8, cf252) ]
      , fuel "LECf-251" 48.3 225 120 (loweff cf251 cf252)
          [ (4, cf251), (20, cf252), (20, cf252), (20, cf252) ]
      , fuel "HECf-251" 48.3 900 720 (higheff cf251 cf252)
          [ (16, cf251), (16, cf252), (16, cf252), (16, cf252) ]
      ]
    
    -- we ignore the oxide fuels because they are equivalent to their non-oxide
    -- counterparts for this specific problem, and so leaving them out reduces the
    -- dimensionality of the linear program without loss of generality
    
    -- fuels = fuels1 ++ (map oxide fuels1) ++ mox
    fuels = fuels1 ++ mox

  in (fissiles, fuels)
  
-- maps are being used here basically to implement sparse vectors
-- nested maps implement sparse arrays
-- TODO: use a proper sparse array type?
  
burn :: Fuel -> Maybe (Step, Map Fissile Int)
burn fuel =
  let
    input = fuelComponents fuel
    output = fuelReprocess fuel
    subtract = merge
        preserveMissing
        (mapMissing (\_ a -> (negate a)))
        (zipWithMatched (\_ x y -> (x - y)))
  in
    Just (Burn fuel, (subtract output input))

decay :: Fissile -> Maybe (Step, Map Fissile Int)
decay fissile =
  case (fissileDecaysTo fissile) of
    Just f -> Just (Decay fissile, fromList [(fissile, -1), (f,1)])
    Nothing -> Nothing

-- build a list of all possible steps    
allsteps :: [(Step, Map Fissile Int)]
allsteps = allburns ++ alldecays
  where allburns = catMaybes (map burn fuels)
        alldecays = catMaybes (map decay fissiles)
        
-- the linear program used here optimizes over the number of each step to perform, with 
-- the objective function being the total RTG RF/t yielded by products left over at 
-- the end

-- transpose a step (horizonal) vector into a fissile (vertical) vector 
transpose :: (Step, Map Fissile Int) -> Map Fissile (Map Step Int)
transpose (step, m) =
  M.map (\n -> (singleton step n)) m

-- combine the vertical vectors generated by transpose into a constraint matrix  
combine :: [Map Fissile (Map Step Int)] -> Map Fissile (Map Step Int)
combine mm = foldr m empty mm
  where m = merge preserveMissing preserveMissing (zipWithMatched m')
        m' _ a b = M.unionWith (+) a b

-- multiply the constraint matrix by the objective weights to get the objective function
recombine :: Map Fissile (Map Step Int) -> Map Step Double
recombine mtx = M.foldrWithKey c empty mtx
  where
    c :: Fissile -> Map Step Int -> Map Step Double -> Map Step Double 
    c f m1 m =
          let mult = fissileRtg f
              m1' = M.map (\n -> mult * ((fromInteger . toInteger) n)) m1
          in M.unionWith (+) m1' m

-- generate the linear program          
mkprob :: [(Step, Map Fissile Int)] -> Map Fissile Int -> ([Double], [Bound [Double]])
mkprob allsteps inv = let
  steps = map fst allsteps
  matrix :: Map Fissile (Map Step Int)
  matrix = combine (map transpose allsteps)
  cvt :: Int -> Double
  cvt = fromInteger . toInteger
  mkConstraint :: Fissile -> Map Step Int -> Bound [Double]
  mkConstraint f one =
    (map (\step -> cvt (findWithDefault 0 step one)) steps)
    :>=: cvt (negate (findWithDefault 0 f inv))
  constraints = (M.elems (M.mapWithKey mkConstraint matrix))
  matrix' :: Map Step Double
  matrix' = recombine matrix
  prob = (map (\step -> findWithDefault 0.0 step matrix') steps)
  in (prob, constraints)

-- actually solve the linear program
solve :: [(Step, Map Fissile Int)] -> Map Fissile Int -> Maybe (Double, [(Step, Double)])
solve allsteps inv = let
  steps = map fst allsteps
  (prob, constraints) = mkprob allsteps inv
  sol = simplex (Maximize prob) (Dense constraints) []
  in case sol of
    Optimal (v, vec) -> Just (v, filter (\x -> ((snd x) > 0)) (zip steps vec))
    otherwise        -> Nothing

