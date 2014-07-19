module Solution
  ( Observation(..)
  , solution
  ) where

----------------------------------------
-- STDLIB
----------------------------------------
import Data.Ratio
import Data.Bits
import Data.Word
import Safe
import Debug.Trace
import Data.Function (on)

type ObsHash = Word8

data Observation
    = Observation
    { obs_probability :: Double
    -- ^ Probability that a random element would be selected for hashing.
    , obs_serverA :: ObsHash -- ^ Hash received from server A
    , obs_serverB :: ObsHash -- ^ Hash received from server B
    }
    deriving Show

observationEqual :: Observation -> Bool
observationEqual obs = obs_serverA obs == obs_serverB obs

maximumBy2 :: (a -> a -> Ordering) -> a -> a -> a
maximumBy2 comp a b | comp a b == LT = b
                    | otherwise      = a
{-# INLINE maximumBy2 #-}

-- | Returns the maximum likelihood estimate for the count of elements
-- that are not on both servers.
solution :: [Observation] -> Maybe Int
solution obss = if null equalObs then Nothing else Just (go (0, 0) 0)
  where
    equalObs = map obs_probability $ filter observationEqual obss
    notEqualObs = map obs_probability $ filter (not . observationEqual) obss
    go :: (Double, Int) -> Int -> Int
    go m@(currentMaxProb, maxK) k =
      let p1 = product $ map (flip probabilityOfInequality k) notEqualObs
          p2 = product $ map (flip probabilityOfEquality k) equalObs
      in if p2 <= currentMaxProb then maxK else go (maximumBy2 (compare `on` fst) m (p1*p2, k+1)) (k+1)

invert :: Double -> Double
invert = (-) 1
{-# INLINE invert #-}

-- | Calculates @P(E_i)@ (approximately) given @p_i@ and @m@
probabilityOfInequality :: Double -> Int -> Double
probabilityOfInequality p m = (1 - (1-p) ^^ m)
{-# INLINE probabilityOfInequality #-}

-- | Calculates @P(F_i)@ (approximately) given @p_i@ and @m@
probabilityOfEquality :: Double -> Int -> Double
probabilityOfEquality p m = invert (probabilityOfInequality p m)
{-# INLINE probabilityOfEquality #-}
