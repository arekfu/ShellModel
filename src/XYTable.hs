module XYTable
( xyTable
, XYTable()
, Table(..)
) where

import qualified Data.Vector.Unboxed as V

data XYTable = XYT (V.Vector Double) (V.Vector Double) deriving (Show, Eq)

xyTable :: V.Vector Double -> V.Vector Double -> XYTable
xyTable xs ys = let n = min (V.length xs) (V.length ys)
                 in XYT (V.take n xs) (V.take n ys)

class Table a where
    (!) :: a -> Int -> (Double, Double)
    findXIndex ::(Double -> Bool) ->  a -> Maybe Int
    findYIndex ::(Double -> Bool) ->  a -> Maybe Int
    findXYIndex ::((Double, Double) -> Bool) ->  a -> Maybe Int
    findX ::(Double -> Bool) ->  a -> Maybe (Double, Double)
    findX property vl = do i <- findXIndex property vl
                           return $ vl ! i
    findY ::(Double -> Bool) ->  a -> Maybe (Double, Double)
    findY property vl = do i <- findYIndex property vl
                           return $ vl ! i
    findXY ::((Double, Double) -> Bool) ->  a -> Maybe (Double, Double)
    findXY property vl = do i <- findXYIndex property vl
                            return $ vl ! i

instance Table XYTable where
    (!) (XYT xs ys) n = (xs V.! n, ys V.! n)
    findXIndex property (XYT xs _) = V.findIndex property xs
    findYIndex property (XYT _ ys) = V.findIndex property ys
    findXYIndex property (XYT xs ys) = V.findIndex property $ V.zip xs ys
