import Graphics.Gnuplot.Simple

type Coord1 = Double
type Coord2 = (Double, Double)

type Bound = (Double, Double)
type V = Double

type Point1 = (Coord1, V)
type Point2 = (Coord2, V)

-- interpolate1 :: Fractional a => a -> a -> a -> a -> a -> a
interpolate1 :: Point1 -> Point1 -> Coord1 -> V
interpolate1 (l, lv) (r, rv) x = lv + steep * (x - l)
  where
    steep = (rv - lv) / (r - l)


interpolate2 :: Bound -> Bound -> (V, V, V, V) -> Coord2 -> V
interpolate2 (a0, a1) (b0, b1) (a0b0, a0b1, a1b0, a1b1) (ax, bx)
  | not (a0 < a1)              = error "a bounds invalid"
  | not (b0 < b1)              = error "b bounds invalid"
  | not (a0 <= ax && ax <= a1) = error $ "ax not in bounds " ++ show ax
  | not (b0 <= bx && bx <= b1) = error $ "bx not in bounds " ++ show bx
  | otherwise                  = f
  where
    -- From: http://en.wikipedia.org/wiki/Bilinear_interpolation
    f = ( a0b0 * (a1 - ax) * (b1 - bx)
        + a1b0 * (ax - a0) * (b1 - bx)
        + a0b1 * (a1 - ax) * (bx - b0)
        + a1b1 * (ax - a0) * (bx - b0)
        ) / ((a1 - a0) * (b1 - b0))

-- For the bounds:
--       b0   b1
--       |    |
-- a1-- 1,0  1,1                 2   3
--                 with heights
-- a0-- 0,0  0,1                 1   4
interpolate2test1 = interpolate2 (0,1) (0,1) (1, 4, 2, 3)

test = and
  -- Corners
  [ interpolate2test1 (0, 0) == 1
  , interpolate2test1 (0, 1) == 4
  , interpolate2test1 (1, 0) == 2
  , interpolate2test1 (1, 1) == 3

  -- Mid-edges
  , interpolate2test1 (0  , 0.5) == 2.5
  , interpolate2test1 (0.5, 0  ) == 1.5
  , interpolate2test1 (0.5, 1  ) == 3.5
  , interpolate2test1 (1  , 0.5) == 2.5

  -- Middle
  , interpolate2test1 (0.5, 0.5) == 2.5
  ]


main = let xs = [0,0.1..0.9::Double] ++ [1.0]
        -- in plotFunc3d [] [] xs xs (\x y -> exp(-(x*x+y*y)))
        in plotFunc3d [] [] xs xs (\x y -> interpolate2test1 (x, y))
