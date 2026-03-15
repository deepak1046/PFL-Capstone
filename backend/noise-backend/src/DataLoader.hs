module DataLoader where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector                as V
import Data.Csv (decodeByName)
import Types

removeBlankLines :: BL.ByteString -> BL.ByteString
removeBlankLines =
    BLC.unlines
  . filter (not . BLC.null)
  . map stripCR
  . BLC.lines
  where
    stripCR bs
      | not (BLC.null bs) && BLC.last bs == '\r' = BLC.init bs
      | otherwise                                  = bs

loadNoiseData :: FilePath -> IO (Int, V.Vector NoiseRecord)
loadNoiseData file = do
  rawCsv <- BL.readFile file
  let csvData = removeBlankLines rawCsv
  case decodeByName csvData of
    Left err -> do
      putStrLn $ "[DataLoader] WARNING: CSV parse error in '" ++ file ++ "'"
      putStrLn $ "[DataLoader] Error detail: " ++ err
      putStrLn   "[DataLoader] Returning empty dataset (server continues)."
      return (0, V.empty)
    Right (_, v) -> do
      let n = V.length v
      putStrLn $ "[DataLoader] Loaded " ++ show n ++ " rows from '" ++ file ++ "'"
      return (n, v)