{-# LANGUAGE OverloadedStrings #-}

module API where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Data.Aeson (ToJSON, object, (.=))
import System.Environment (lookupEnv)

-- | Allow any origin so the frontend (GitHub Pages / local file / Netlify
--   etc.) can call us without CORS failures.
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
  { corsOrigins        = Nothing                              -- allow all origins
  , corsMethods        = ["GET", "POST", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization", "Accept"]
  }

-- | Start the Scotty web server.
--
--   averages   :: Map Text Double  — per-city average noise, used by /noise/average
--   varianceVal :: Double          — global sample variance, used by /noise/variance
--   rowCount    :: Int             — total rows loaded, exposed on /noise/health
startServer :: (ToJSON a, ToJSON b) => a -> b -> Int -> IO ()
startServer averages varianceVal rowCount = do
  envPort <- lookupEnv "PORT"
  let port = maybe 3000 read envPort
  putStrLn $ "[API] Server running on port " ++ show port

  scotty port $ do

    middleware $ cors (const $ Just corsPolicy)

    -- -----------------------------------------------------------------------
    -- GET /noise/average
    -- Returns: {"Delhi": 84.3, "Mumbai": 80.1, ...}
    -- Used by the frontend bar chart and doughnut chart.
    -- -----------------------------------------------------------------------
    get "/noise/average" $
      json averages

    -- -----------------------------------------------------------------------
    -- GET /noise/variance
    -- Returns: 48.72   (a plain JSON number)
    -- Global sample variance across all noise readings.
    -- -----------------------------------------------------------------------
    get "/noise/variance" $
      json varianceVal

    -- -----------------------------------------------------------------------
    -- GET /noise/health
    -- Returns: {"status":"ok","rows":120}
    -- Useful for deployment health-checks and debugging.
    -- -----------------------------------------------------------------------
    get "/noise/health" $
      json $ object
        [ "status" .= ("ok" :: String)
        , "rows"   .= rowCount
        ]