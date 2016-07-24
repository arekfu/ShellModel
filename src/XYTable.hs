module XYTable
( xyTable
, (!)
) where

import qualified Data.Vector.Unboxed as V

data XYTable = XYT (V.Vector Double) (V.Vector Double)

xyTable :: V.Vector Double -> V.Vector Double -> XYTable
xyTable xs ys = let n = min (V.length xs) (V.length ys)
                 in XYT (V.take n xs) (V.take n ys)

(!) :: XYTable -> Int -> (Double, Double)
(!) (XYT xs ys) n = (xs V.! n, ys V.! n)
