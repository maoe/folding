module Main (main) where

import Criterion.Main
import Prelude as Prelude
import qualified Control.Foldl as Foldl
import qualified Control.Fold as Folding
import qualified Data.List as List

main :: IO ()
main = defaultMain
  [ env (return [1..10000 :: Int]) $ \ns ->
    bgroup "[1..10000 :: Int]"
      [ bgroup "sum" $ map ($ ns)
        [ bench "Foldl.fold sum"
          . whnf (Foldl.fold Foldl.sum)
        , bench "Folding.fold sum"
          . whnf (Folding.fold Folding.sum)
        , bench "Foldl.foldM (generalize sum)"
          . whnfIO . Foldl.foldM (Foldl.generalize Foldl.sum)
        , bench "Prelude.sum"
          . whnf Prelude.sum
        , bench "Data.List.foldl' (+) 0"
          . whnf (List.foldl' (+) 0)
        ]
      , bgroup "length" $ map ($ ns)
        [ bench "Foldl.fold length"
          . whnf (Foldl.fold Foldl.length)
        , bench "Folding.fold length"
          . whnf (Folding.fold Folding.length)
        , bench "foldM (generalize length)"
          . whnfIO . Foldl.foldM (Foldl.generalize Foldl.length)
        , bench "Prelude.length"
          . whnf Prelude.length
        ]
      ]
  ]
