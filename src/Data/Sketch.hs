{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Sketch where

{-| A sketch that summarizes a stream element i, and maintains a summary o.
 
- A sketch is an approximation of a summary or a query. A sketch 
- provides an answer within an accuracy 'e' and  with probability
- 'delta'. 
-}
class Sketch s i o where

  -- Initialize a sketch given an error, e 
  initialize :: Double -> Double -> [s i o]

  -- Update a single copy sketch with a
  update :: s i o -> i -> s i o

  -- Update all copies with input i
  updateAll :: [s i o] -> i -> [s i o]
  updateAll ss i = flip update i <$> ss

  
