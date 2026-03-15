module Main where

import DataLoader  (loadNoiseData)
import DataCleaner (cleanData)
import Analysis    (averageByLocation, cityCount, cleanedCount)
import Sensitivity (noiseVariance)
import API         (startServer)

main :: IO ()
main = do
  -- Load CSV once at startup — never crashes on bad data
  (rowCount, dataset) <- loadNoiseData "dataset/cpcb_noise.csv"

  -- Drop rows with implausible noise levels (<=0 or >=200 dB)
  let cleaned = cleanData dataset

  -- Pre-compute all analytics once; serve from memory on every request
  let averages    = averageByLocation cleaned
  let varianceVal = noiseVariance     cleaned

  putStrLn $ "[Main] Raw rows loaded:   " ++ show rowCount
  putStrLn $ "[Main] Rows after clean:  " ++ show (cleanedCount cleaned)
  putStrLn $ "[Main] Cities found:      " ++ show (cityCount averages)
  putStrLn $ "[Main] Global variance:   " ++ show varianceVal

  -- Start Scotty server (port from $PORT env var or 3000 locally)
  startServer averages varianceVal rowCount