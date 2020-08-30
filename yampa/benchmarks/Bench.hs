-- |
-- A benchmark for Yampa.
module Main where

import Criterion      (bench, bgroup, nf)
import Criterion.Main (defaultMain)

import FRP.Yampa

-- At present, this simply contains two implementations of the same SF. It is
-- just an initial way of doing this.

main :: IO ()
main = defaultMain
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
