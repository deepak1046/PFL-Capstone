module Main where

import DataLoader
import DataCleaner
import Analysis
import Sensitivity
import API

main :: IO ()
main = do
  dataset <- loadNoiseData "dataset/cpcb_noise.csv"
  let cleaned = cleanData dataset
  let averages = averageByLocation cleaned
  let varianceVal = noiseVariance cleaned

  startServer averages varianceVal