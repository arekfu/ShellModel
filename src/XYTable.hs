module XYTable
( xyTable
, XYTable()
, (!)
, findX
, findY
, findXY
, findXIndex
, findYIndex
, findXYIndex
) where

import qualified Data.Vector.Unboxed as V

data XYTable = XYT (V.Vector Double) (V.Vector Double) deriving (Show, Eq)

xyTable :: V.Vector Double -> V.Vector Double -> XYTable
xyTable xs ys = let n = min (V.length xs) (V.length ys)
                 in XYT (V.take n xs) (V.take n ys)

(!) :: XYTable -> Int -> (Double, Double)
(!) (XYT xs ys) n = (xs V.! n, ys V.! n)

findXIndex ::(Double -> Bool) ->  XYTable -> Maybe Int
findXIndex property (XYT xs _) = V.findIndex property xs

findYIndex ::(Double -> Bool) ->  XYTable -> Maybe Int
findYIndex property (XYT _ ys) = V.findIndex property ys

findXYIndex ::((Double, Double) -> Bool) ->  XYTable -> Maybe Int
findXYIndex property (XYT xs ys) = V.findIndex property $ V.zip xs ys

findX ::(Double -> Bool) ->  XYTable -> Maybe (Double, Double)
findX property xy = do i <- findXIndex property xy
                       return $ xy ! i

findY ::(Double -> Bool) ->  XYTable -> Maybe (Double, Double)
findY property xy = do i <- findYIndex property xy
                       return $ xy ! i

findXY::((Double, Double) -> Bool) ->  XYTable -> Maybe (Double, Double)
findXY property xy = do i <- findXYIndex property xy
                        return $ xy ! i
