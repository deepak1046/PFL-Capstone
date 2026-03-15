{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Aeson   (ToJSON)
import Data.Csv     (FromNamedRecord, parseNamedRecord, (.:))
import Data.Text    (Text)
import Control.Applicative ((<|>))

-- ---------------------------------------------------------------------------
-- | One row from cpcb_noise.csv
-- ---------------------------------------------------------------------------
data NoiseRecord = NoiseRecord
  { station    :: !Text
  , year       :: !Int
  , month      :: !Int
  , day        :: !Double
  , night      :: !Double
  , dayLimit   :: !Int
  , nightLimit :: !Int
  , name       :: !Text
  , city       :: !Text
  , state      :: !Text
  , noiseType  :: !Text
  } deriving (Show, Generic)

instance ToJSON NoiseRecord

-- | Helper to parse empty string "" as 0.0, or try normal Double parse.
--   Cassava's default behavior fails on empty strings for numeric fields.
instance FromNamedRecord NoiseRecord where
  parseNamedRecord r =
    NoiseRecord
      <$> r .: "Station"
      <*> r .: "Year"
      <*> r .: "Month"
      <*> (r .: "Day"   <|> pure 0.0)  -- Default to 0.0 if empty / invalid
      <*> (r .: "Night" <|> pure 0.0)  -- Default to 0.0 if empty / invalid
      <*> r .: "DayLimit"
      <*> r .: "NightLimit"
      <*> r .: "Name"
      <*> r .: "City"
      <*> r .: "State"
      <*> r .: "Type"