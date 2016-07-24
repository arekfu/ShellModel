module FuncTypes
( Potential()
, mkPotential
, WaveFunction
, mkWaveFunction
, range
, woodsSaxonPot
) where

import XYTable
import qualified Data.Vector.Unboxed as V

newtype Potential = Potential XYTable deriving (Show, Eq)

mkPotential :: V.Vector Double -> V.Vector Double -> Potential
mkPotential xs ys = Potential $ xyTable xs ys

newtype WaveFunction = WaveFunction XYTable deriving (Show, Eq)

mkWaveFunction :: V.Vector Double -> V.Vector Double -> WaveFunction
mkWaveFunction xs ys = WaveFunction $ xyTable xs ys

lRange :: (Num a, Fractional a) => Int -> a -> a -> [a]
lRange n xMin xMax = map (\i -> xMin + (xMax-xMin)* fromIntegral i/fromIntegral (n-1)) [0..n-1]

range :: (V.Unbox a, Fractional a) => Int -> a -> a -> V.Vector a
range n xMin xMax = V.fromList $ lRange n xMin xMax

woodsSaxon :: Double -> Double -> Double -> Double -> Double
woodsSaxon r0 a v0 r = v0 / (1.0 + exp (r-r0)/a)

woodsSaxonPot :: Int -> Double -> Double -> Double -> Double -> Potential
woodsSaxonPot n rMax r0 a v0 = let xs = range n 0.0 rMax
                                   ys = V.map (woodsSaxon r0 a v0) xs
                                in mkPotential xs ys
