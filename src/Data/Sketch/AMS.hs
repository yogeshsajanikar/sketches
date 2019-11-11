{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}
module Data.Sketch.AMS where


import Data.Sketch


-- Individual AMS sketch. It contains a pairwise hash function, length of the 
data AMS i o = AMS { hash :: Int -> Int, width :: Int }


instance Sketch AMS i o where

  initialize :: Double -> Double -> [AMS i o]
  initialize err delta | err <= 0.0 || delta <= 0.0 = error "Error and delta must be positive"
  initialize err delta | err > 1.0  || delta > 1.0 = error "Error and delta must lie within [0,1]"

  initialize err delta = const AMS { hash = \x -> x, width = ceiling w }  <$> [1..d] 
    where
      err2 = err * err
      log2 = logBase 2
      w = 4 / err2 -- AMS Frequency moments
      d = 8 * log2  (1 / delta)
    

  update = undefined
