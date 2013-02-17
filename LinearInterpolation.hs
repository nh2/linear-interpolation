{-# LANGUAGE ParallelListComp #-}

import Graphics.Gnuplot.Simple
import Test.QuickCheck
import Test.QuickCheck.Function
import Control.Applicative

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


interpolate2fun :: ((Int, Int) -> V) -> Coord2 -> V
interpolate2fun f (ax, bx) = interpolate2 (fi a0, fi a1) (fi b0, fi b1) (f (a0,b0), f (a0,b1), f (a1,b0), f (a1,b1)) (ax, bx)
  where
    fi = fromIntegral
    a0 = floor ax   :: Int
    a1 = ceiling ax :: Int
    b0 = floor bx   :: Int
    b1 = ceiling bx :: Int


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


interpolate3fun :: (Coord3 -> V) -> Coord3 -> V
interpolate3fun f (ax, bx, cx) = interpolate3 (a0, a1)
                                              (b0, b1)
                                              (c0, c1) ( f (a0,b0,c0)
                                                       , f (a0,b0,c1)
                                                       , f (a0,b1,c0)
                                                       , f (a0,b1,c1)
                                                       , f (a1,b0,c0)
                                                       , f (a1,b0,c1)
                                                       , f (a1,b1,c0)
                                                       , f (a1,b1,c1) ) (ax, bx, cx)
  where
    a0 = fromIntegral (floor ax   :: Int)
    a1 = fromIntegral (ceiling ax :: Int)
    b0 = fromIntegral (floor bx   :: Int)
    b1 = fromIntegral (ceiling bx :: Int)
    c0 = fromIntegral (floor cx   :: Int)
    c1 = fromIntegral (ceiling cx :: Int)


interpolateN :: [Bound] -> [V] -> [Coord] -> V
interpolateN bounds cornerVals coords
  | dim < 1                    = err $ "cannot interpolate in " ++ show dim ++ " dimentions, need at least 1"
  | length coords     /= dim   = err $ "dimension of position to interpolate (" ++ show (length coords) ++ ") does not match bounds dimension (" ++ show dim ++ ")"
  | length cornerVals /= 2^dim = err $ "number of boundary values (" ++ show (length cornerVals) ++ ") does not match bounds dimension (" ++ show dim ++ ")"
  | otherwise                  = f
  where
    err s = error $ "interpolateN: " ++ s
    dim = length bounds

    f = sum [ v * product weights
               | v <- cornerVals
               | weights <- sequence [ [b1-x, x-b0] | (b0, b1) <- bounds
                                                    | x        <- coords ]
               ]
        / product [ b1 - b0 | (b0, b1) <- bounds ]



-- TODO dimension check: dim (f x) == dim coords
interpolateNfun :: ([Int] -> V) -> [Coord] -> V
interpolateNfun f coords = interpolateN (fi2 <$> bounds) cornerVals coords
  where
    fi2 (a, b) = (fromIntegral a, fromIntegral b)
    bounds = [ (floor c, ceiling c) | c <- coords ]
    toLists = map (\(x, y) -> [x, y])
    cornerVals = [ f cornerCoord | cornerCoord <- sequence (toLists bounds) ]




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




qc = quickCheck $ forAll gen01 $ \(x, y) ->
  abs (interpolateNtest2 [x, y] - interpolate2test1 (x, y)) < 1e-6
  where
    gen01 = (,) <$> choose (0, 1) <*> choose (0, 1)


interpolate3test1 = interpolate3 (0,1) (0,1) (5,7) (1, 4, 2, 3, 1, 0, 9, 1)
interpolateNtest3 = interpolateN [(0,1),(0,1),(5,7)] [1, 4, 2, 3, 1, 0, 9, 1]

qc3 = quickCheck $ forAll gen01 $ \(x, y, z) ->
  abs (interpolateNtest3 [x, y, z] - interpolate3test1 (x, y, z)) < 1e-6
  where
    gen01 = (,,) <$> choose (0, 1) <*> choose (0, 1) <*> choose (5, 7)


main = let xs = [0,0.1..0.9::Double] ++ [1.0]
        -- in plotFunc3d [] [] xs xs (\x y -> exp(-(x*x+y*y)))
        in plotFunc3d [] [] xs xs (\x y -> interpolate2test1 (x, y))


prop :: Fun String Integer -> Bool
prop (Fun _ f) = f "monkey" == f "banana" || f "banana" == f "elephant"


propAnyF :: Fun (Int, Int) Double -> Gen Bool
propAnyF (Fun _ f) = do
  (a, b) <- arbitrary
  let list2tup [a, b] = (a, b)
  return $ interpolate2fun f (a, b) == interpolateNfun (f . list2tup) [a, b]
