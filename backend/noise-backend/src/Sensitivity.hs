module Sensitivity where

import qualified Data.Vector       as V
import qualified Statistics.Sample as S
import Types

-- ---------------------------------------------------------------------------
-- | Global sample variance of the combined noise level (day+night)/2
--   across ALL records in the dataset.
--
--   Uses Statistics.Sample.variance (Welford's online algorithm internally),
--   which is numerically stable and efficient for large vectors.
--
--   Returns 0.0 for empty datasets to avoid NaN in the JSON response.
-- ---------------------------------------------------------------------------
noiseVariance :: V.Vector NoiseRecord -> Double
noiseVariance records
  | V.null records = 0.0
  | otherwise      = S.variance combined
  where
    combined = V.map (\r -> (day r + night r) / 2.0) records

-- ---------------------------------------------------------------------------
-- | Global mean of (day+night)/2 across all records.
-- ---------------------------------------------------------------------------
noiseMean :: V.Vector NoiseRecord -> Double
noiseMean records
  | V.null records = 0.0
  | otherwise      = S.mean (V.map (\r -> (day r + night r) / 2.0) records)