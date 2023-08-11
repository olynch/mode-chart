module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

dividedRing :: Double -> Double -> [String] -> Diagram B
dividedRing outer inner labels = mconcat $ segment <$> zip labels [0..]
  where
    k :: Int
    k = length labels
    dθ :: Angle Double
    dθ = (tau / fromIntegral (2 * k)) @@ rad
    dθ2 = dθ <> dθ
    θ :: Int -> Angle Double
    θ i = (fromIntegral i * tau / fromIntegral k) @@ rad
    middle :: Double
    middle = (outer + inner) / 2
    segment :: (String, Int) -> Diagram B
    segment (s, i) =
      annularWedge outer inner (rotate (θ i) xDir) dθ2
        <> (text s # translateX middle # rotate (θ i <> dθ))

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

example :: Diagram B
example = (chart <> labels) # lw 0.1
  where
    chart :: Diagram B
    chart = mconcat $ mkRing <$> zip rows [0..]
    labels :: Diagram B
    labels = dividedRing (radius) (radius - 6) (take 84 modes)
    notes :: [String]
    notes = rotateList 4 ["A", "B♭", "B", "C", "C♯", "D", "E♭", "E", "F", "F♯", "G", "G♯"]
    modes :: [String]
    modes = cycle ["Locrian", "Phrygian", "Aeolian", "Dorian", "Mixolydian", "Ionian", "Lydian"]
    notesRepeated :: [String]
    notesRepeated = concat [replicate 7 note | note <- notes]
    rows :: [[String]]
    rows = [ rotateList i notesRepeated | i <- [0,12..72] ]
    dr :: Double
    dr = 1.5
    radius :: Double
    radius = 30
    mkRing :: ([String], Int) -> Diagram B
    mkRing (row, i) = dividedRing (radius + (i'+1) * dr) (radius + i' * dr) row
      where
        i' = fromIntegral i

main :: IO ()
main = mainWith example
