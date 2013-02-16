{-# LANGUAGE ParallelListComp #-}

import Graphics.Gnuplot.Simple

type Coord = Double
type Coord1 = Double
type Coord2 = (Double, Double)
type Coord3 = (Double, Double, Double)

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
    a_rest = a1 - ax
    b_rest = b1 - bx
    a_dist = ax - a0
    b_dist = bx - b0
    f = ( a0b0 * a_rest * b_rest
        + a1b0 * a_dist * b_rest
        + a0b1 * a_rest * b_dist
        + a1b1 * a_dist * b_dist
        ) / ((a1 - a0) * (b1 - b0))

interpolate3 :: Bound -> Bound -> Bound -> (V, V, V, V, V, V, V, V) -> Coord3 -> V
interpolate3 (a0, a1) (b0, b1) (c0, c1) ( a0b0c0
                                        , a0b0c1
                                        , a0b1c0
                                        , a0b1c1
                                        , a1b0c0
                                        , a1b0c1
                                        , a1b1c0
                                        , a1b1c1) (ax, bx, cx)
  | not (a0 < a1)              = error "a bounds invalid"
  | not (b0 < b1)              = error "b bounds invalid"
  | not (c0 < c1)              = error "c bounds invalid"
  | not (a0 <= ax && ax <= a1) = error $ "ax not in bounds " ++ show ax
  | not (b0 <= bx && bx <= b1) = error $ "bx not in bounds " ++ show bx
  | not (c0 <= cx && cx <= c1) = error $ "cx not in bounds " ++ show cx
  | otherwise                  = f
  where
    ar = a1 - ax
    br = b1 - bx
    cr = c1 - cx
    a_ = ax - a0
    b_ = bx - b0
    c_ = cx - c0
    f = ( a0b0c0 * ar * br * cr
        + a1b0c0 * a_ * br * cr
        + a0b1c0 * ar * b_ * cr
        + a1b1c0 * a_ * b_ * cr
        + a0b0c1 * ar * br * c_
        + a1b0c1 * a_ * br * c_
        + a0b1c1 * ar * b_ * c_
        + a1b1c1 * a_ * b_ * c_
        ) / ((a1 - a0) * (b1 - b0) * (c1 - c0))


interpolateN :: [Bound] -> [V] -> [Coord] -> V
interpolateN bounds vals coords
  | dim < 1              = err $ "cannot interpolate in " ++ show dim ++ " dimentions, need at least 1"
  | length coords /= dim = err $ "dimension of position to interpolate (" ++ show (length coords) ++ ") does not match bounds dimension (" ++ show dim ++ ")"
  | length vals /= 2^dim = err $ "number of boundary values (" ++ show (length vals) ++ ") does not match bounds dimension (" ++ show dim ++ ")"
  | otherwise            = f
  where
    err s = error $ "interpolateN: " ++ s
    dim = length bounds

    f = sum [ v * product weights
               | v <- vals
               | weights <- sequence [ [b1-x, x-b0] | (b0, b1) <- bounds
                                                    | x        <- coords ]
               ]
        / product [ b1 - b0 | (b0, b1) <- bounds ]


interpolateNtest1 = interpolateN [(0,1)] [0,1]

testsN1 = and
  [ interpolateNtest1 [0  ] == 0
  , interpolateNtest1 [1  ] == 1
  , interpolateNtest1 [0.5] == 0.5
  ]

interpolateNtest2 = interpolateN [(0,1),(0,1)] [1, 4, 2, 3]

testN2 = and
  -- Corners
  [ interpolateNtest2 [0, 0] == 1
  , interpolateNtest2 [0, 1] == 4
  , interpolateNtest2 [1, 0] == 2
  , interpolateNtest2 [1, 1] == 3

  -- Mid-edges
  , interpolateNtest2 [0  , 0.5] == 2.5
  , interpolateNtest2 [0.5, 0  ] == 1.5
  , interpolateNtest2 [0.5, 1  ] == 3.5
  , interpolateNtest2 [1  , 0.5] == 2.5

  -- Middle
  , interpolateNtest2 [0.5, 0.5] == 2.5
  ]


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
