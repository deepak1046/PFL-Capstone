module DataCleaner where

import qualified Data.Vector as V
import Types

-- ---------------------------------------------------------------------------
-- | Remove rows with clearly invalid Day or Night noise readings.
--
--   Valid range for environmental noise: 20 dB(A) – 120 dB(A)
--   • Below 20 dB: sensor error / no reading
--   • Above 120 dB: sensor overflow / data corruption
-- ---------------------------------------------------------------------------
cleanData :: V.Vector NoiseRecord -> V.Vector NoiseRecord
cleanData = V.filter validRow
  where
    validRow r =
      day   r >  20.0 && day   r < 120.0 &&
      night r >  20.0 && night r < 120.0