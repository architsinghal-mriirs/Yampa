-- |
-- A benchmark for Yampa.
module Main where

import Criterion           (bench, bgroup, nf)
import Criterion.Main      (defaultConfig, defaultMainWith)
import Criterion.Types     (Config(csvFile, resamples, verbosity)
                           , Verbosity(Quiet))
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format    (formatTime, defaultTimeLocale)
import System.Environment  (getArgs, withArgs)
import System.FilePath     ((</>))

import FRP.Yampa

-- At present, this simply contains two implementations of the same SF. It is
-- just an initial way of doing this.

main :: IO ()
main = do
  config <- customConfig
  withArgs [] $
    defaultMainWith config
       [ bgroup "counter"
                [ bench "counter1" $ nf counter1 100
                , bench "counter2" $ nf counter2 100
                ]
       ]

counter1 :: Int -> [Int]
counter1 n = embed sf stream
  where
    sf     = loopPre 0 (arr (dup . uncurry (+)))
    stream = deltaEncode 1.0 (replicate n 1)

counter2 :: Int -> [Int]
counter2 n = embed sf stream
  where
    sf     = loopPre 0 (arr ((\x -> x `seq` (x, x)). uncurry (+)))
    stream = deltaEncode 1.0 (replicate n 1)

-- * Auxiliary functions

-- Construct a config with increased number of sampling
-- and a custom name for the report.
customConfig :: IO Config
customConfig = do
  args <- getArgs

  let dir = case args of
              []     -> "."
              (x:xs) -> x

  -- Custom filename using the current time
  timeString <- (formatTime defaultTimeLocale "%F-%H%M%S") <$> getZonedTime
  let filename = concat [ timeString, "-", "bench.csv" ]

  return $ defaultConfig { csvFile   = Just $ dir </> filename
                         , resamples = 100000
                         , verbosity = Quiet
                         }
