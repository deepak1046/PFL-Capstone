module Analysis where

import qualified Data.Vector     as V
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Types

-- ---------------------------------------------------------------------------
-- Math helpers
-- ---------------------------------------------------------------------------

-- | Safe mean — returns 0.0 for an empty list.
avg :: [Double] -> Double
avg [] = 0.0
avg xs = sum xs / fromIntegral (length xs)

-- | Bessel-corrected sample variance (n-1 denominator).
--   Returns 0.0 when < 2 elements to avoid divide-by-zero.
var :: [Double] -> Double
var xs
  | length xs < 2 = 0.0
  | otherwise     =
      let m      = avg xs
          n      = fromIntegral (length xs) - 1   -- n-1
          sq x   = (x - m) ^ (2 :: Int)
      in  sum (map sq xs) / n

-- ---------------------------------------------------------------------------
-- Grouping
-- ---------------------------------------------------------------------------

-- | Group records by City, collecting both Day and Night readings.
--   Each city maps to a list of (day, night) pairs.
groupByCity :: V.Vector NoiseRecord -> M.Map Text [(Double, Double)]
groupByCity =
  V.foldl'
    (\m r -> M.insertWith (++) (city r) [(day r, night r)] m)
    M.empty

-- ---------------------------------------------------------------------------
-- Public analytics
-- ---------------------------------------------------------------------------

-- | Average noise per city = mean of (day + night) / 2 across all records.
--   Returns {"Bengaluru": 64.2, "Delhi": 70.1, ...}
--   This is the format the frontend chart consumes.
averageByLocation :: V.Vector NoiseRecord -> M.Map Text Double
averageByLocation records =
  let grouped = groupByCity records
  in  fmap (\pairs -> avg [ (d + n) / 2 | (d, n) <- pairs ]) grouped

-- | Sample variance per city — based on the combined (day+night)/2 readings.
varianceByLocation :: V.Vector NoiseRecord -> M.Map Text Double
varianceByLocation records =
  let grouped = groupByCity records
  in  fmap (\pairs -> var [ (d + n) / 2 | (d, n) <- pairs ]) grouped

-- | Helper: count cleaned records (wrapped to avoid exe needing Data.Vector)
cleanedCount :: V.Vector NoiseRecord -> Int
cleanedCount = V.length

-- | Helper: count distinct cities in the averages map
cityCount :: M.Map Text Double -> Int
cityCount = M.size