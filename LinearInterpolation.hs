import Debug.Trace

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


-- interpolate2 :: Point2 -> Point2 -> Point2 -> Point2 -> Coord2 -> Double
-- interpolate2 ((a1, b1), v1) ((a2, b2), v2) ((a3, b3), v3) ((a4, b4), v4) (ax, bx)

interpolate2 :: Bound -> Bound -> (V, V, V, V) -> Coord2 -> V
interpolate2 (a0, a1) (b0, b1) (a0b0, a0b1, a1b0, a1b1) (ax, bx)
  | not (a0 < a1)              = error "a bounds invalid"
  | not (b0 < b1)              = error "b bounds invalid"
  | not (a0 <= ax && ax <= a1) = error "ax not in bounds"
  | not (b0 <= bx && bx <= b1) = error "bx not in bounds"
  | otherwise                  = traceShow ("dista", dista) $
                                 traceShow ("distb", distb) $
                                 traceShow ("distax", distax) $
                                 traceShow ("distbx", distbx) $
                                 traceShow ("fraca", fraca) $
                                 traceShow ("fracb", fracb) $
                                 traceShow ("steepOna0", steepOna0) $
                                 traceShow ("steepOna1", steepOna1) $
                                 traceShow ("steepOnb0", steepOnb0) $
                                 traceShow ("steepOnb1", steepOnb1) $
                                 -- traceShow ("a0b0 + distax * steepOna1 * fraca", a0b0 + distax * steepOna1 * fraca) $
                                 -- traceShow ("a1b0 + distax * steepOna0 * (1 - fraca)", a1b0 + distax * steepOna0 * (1 - fraca)) $
                                 -- traceShow ("a0b0 + distbx * steepOnb1 * fracb", a0b0 + distbx * steepOnb1 * fracb) $
                                 -- traceShow ("a0b1 + distbx * steepOnb0 * (1 - fracb)", a0b1 + distbx * steepOnb0 * (1 - fracb)) $
                                 traceShow ("starta0", starta0) $
                                 traceShow ("startb0", startb0) $
                                 traceShow ("enda1", enda1) $
                                 traceShow ("endb1", endb1) $
                                 -- traceShow ("", ) $
                                 f
  where
    dista = a1 - a0
    distb = b1 - b0
    distax = ax - a0
    distbx = bx - b0
    fraca     = distax / dista
    fracb     = distbx / distb

    steepOna0 = (a0b1 - a0b0) / distb
    steepOna1 = (a1b1 - a1b0) / distb
    steepOnb0 = (a1b0 - a0b0) / dista
    steepOnb1 = (a1b1 - a0b1) / dista

    starta0 = a0b0 + distbx * steepOna0
    startb0 = a0b0 + distax * steepOnb0

    enda1 = a1b0 + distbx * steepOna1
    endb1 = a0b1 + distax * steepOnb1

    f = ( ( starta0 * (1-fraca) + enda1 * fraca ) +
          ( startb0 * (1-fracb) + endb1 * fracb )
        ) / 2
    -- f = ((a0b0 + distbx * steepOna0) * (1 - fraca) +
    --      (a1b0 + distbx * steepOna1) * fraca        +
    --      (a0b0 + distax * steepOnb0) * (1 - fracb) +
    --      (a0b1 + distax * steepOnb1) * fracb
    --     ) / 4.0
    -- f = (steepOna1 * fraca +
    --      steepOna0 * (1 - fraca) +
    --      steepOnb1 * fracb +
    --      steepOnb0 * (1 - fracb)
    --     ) / 4.0

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
  ]
