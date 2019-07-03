{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, NamedFieldPuns #-}

module Main (main) where

import Prelude
     ( ($), (+), (*), (<)
     , Monoid (mempty), Either (Left, Right), Float, Word, IO
     , const, sin, cos, pi, foldl, error, fromIntegral, abs, seq, snd
     )

import Prelude.Unicode ((∘), (−), (÷), (≥))

import Data.List.NonEmpty as NonEmpty
     ( NonEmpty ((:|)), toList, reverse, cycle, last, zip, drop, length
     )

import Data.Vector.Unboxed as Vector
     ( Vector, fromList, foldl, map, snoc, null, unsafeHead, unsafeTail
     )

import Graphics.Gloss (Display (InWindow), Picture (..), Path, Point, simulate)

import Graphics.Gloss.Data.Color
     ( Color, black, white, light, dark, cyan
     , red, orange, yellow, green, blue, violet
     )

type Length = Float -- ^ Of a line
type Speed  = Float -- ^ Rotation per second
type Rot    = Float -- ^ Current rotation value (where from 0 to 1 is 360 degrees turn)

data State
   = State
   { rotators ∷ !(Vector (Length, Speed, Rot))
   , stops    ∷ !(Vector Point)
   , stopD    ∷ !Float -- ^ Time δa from last step
   }


deg90 ∷ Float
deg90 = 1 ÷ 2 * pi

deg360 ∷ Float
deg360 = deg90 * 4

-- | Infinite cycled rainbow
infRainbow ∷ NonEmpty Color
infRainbow = cycle $ red :| [orange, yellow, green, light blue, blue, violet]

linesPerSec ∷ Float
linesPerSec = 60

stopThreshold ∷ Float
stopThreshold = 1 ÷ linesPerSec


initialState ∷ State
initialState
  = State
  { stops = mempty
  , stopD = 0

  , rotators
      = fromList
      [ (10, 0.7, 0)
      , (25, 0.5, 0)
      , (30, 0.3, 0)
      , (60, 0.7, 0)
      , (15, 1.9, 0)
      , (29, 0.3, 0)
      , (33, 2.5, 0)
      , (88, 2.5, 0)
      , (22, 1.5, 0)
      , (73, 0.7, 0)
      ]
  }


main ∷ IO ()
main = go where
  fpsLimit = 60
  winSize = (800, 600)
  state = initialState { stopD = stopThreshold }
  go = simulate display black fpsLimit state render $ const update
  display = InWindow "Rotating Vectors (Fourier series)" winSize (100, 100)


render ∷ State → Picture
render State { stops, rotators } = go where
  go = Color white $ Pictures $ stopsPic : rotLines
  stopsReducer (a, acc) stop = (stop, Line [a, stop] : acc)
  rotReducer (a, acc) (c, rot) = (rot, Color c (Line [a, rot]) : acc)

  stopsPic =
    if null stops
       then Blank
       else Pictures
          $ snd
          $ Vector.foldl stopsReducer (unsafeHead stops, [])
          $ unsafeTail stops

  rotLines = snd $
    case zip infRainbow $ rotToPoints rotators of
         (_, x) :| xs → Prelude.foldl rotReducer (x, []) xs


update ∷ Float → State → State
update δ state@State { rotators, stops, stopD } = nextState where
  rotatorMapFn (len, speed, rot) = (len, speed, rot + (deg360 * speed * δ))
  nextRotators = map rotatorMapFn rotators
  thisStep = stopD + δ
  diff = stopThreshold − thisStep

  nextState =
    if diff ≥ 0
       then state { rotators = nextRotators, stopD = thisStep }
       else state { rotators = nextRotators, stopD = abs diff
                  , stops = case rotToPoints nextRotators of
                                 x :| _ → snoc stops x
                  }


rotToPoints ∷ Vector (Length, Speed, Rot) → NonEmpty Point
rotToPoints = go where
  go = Vector.foldl reducer ((0, 0) :| [])

  reducer (x@(x', y') :| xs) (l, _, r) =
    (x' + (sin r * l), y' + (cos r * l)) :| (x : xs)
