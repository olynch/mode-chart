module Main where

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

import Data.Colour.Palette.BrewerSet

type Semitones = Int

type NoteLetter = Int

noteLetter :: NoteLetter -> String
noteLetter 0 = "C"
noteLetter 1 = "D"
noteLetter 2 = "E"
noteLetter 3 = "F"
noteLetter 4 = "G"
noteLetter 5 = "A"
noteLetter 6 = "B"
noteLetter l = noteLetter (l `mod` 7)

letterOffset :: NoteLetter -> Semitones
letterOffset 0 = 0
letterOffset 1 = 2
letterOffset 2 = 4
letterOffset 3 = 5
letterOffset 4 = 7
letterOffset 5 = 9
letterOffset 6 = 11
letterOffset l = (l `div` 7) * 12 + letterOffset (l `mod` 7)

nextLetter :: NoteLetter -> NoteLetter
nextLetter l = (l + 1) `mod` 7

type Adjustment = Int

adjustment :: Adjustment -> String
adjustment (-2) = "𝄫"
adjustment (-1) = "♭"
adjustment 0 = ""
adjustment 1 = "♯"
adjustment 2 = "𝄪"
adjustment _ = error "no such adjustment"

type Note = (NoteLetter, Adjustment)

normalNotes :: [Note]
normalNotes = [
  (0,0), -- C
  (0,1), -- Cs
  (1,0), -- D
  (2,-1), -- Eb
  (2,0), -- E
  (3,0), -- F
  (3,1), -- Fs
  (4,0), -- G
  (4,1), -- Gs
  (5,0), -- A
  (6,-1), -- Bb
  (6,0) -- B
  ]

note :: Note -> String
note (l, a) = noteLetter l <> adjustment a

semitones :: Note -> Int
semitones (l, a) = letterOffset l + a

type Mode = Int

ionianSteps :: [Int]
ionianSteps = [2, 2, 1, 2, 2, 2, 1]

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

modeSteps :: Mode -> [Int]
modeSteps m = rotateList m ionianSteps

modeOrder = [3, 0, 4, 1, 5, 2, 6]

-- Calculate the adjustment on the next note needed to raise (l,a) by offset semitones
nextAdjustment :: Note -> Int -> Adjustment
nextAdjustment (l, a) offset = (semitones (l, a) + offset) - letterOffset (l + 1)

nextNote :: Note -> Int -> Note
nextNote (l, a) offset = ((l + 1) `mod` 7, nextAdjustment (l, a) offset)

modeNotes :: Mode -> Note -> [Note]
modeNotes m n = scanl nextNote n steps
  where
    (Just steps) = viaNonEmpty init $ modeSteps m

colors :: [Kolor]
colors = concat [ replicate 7 c | c <- set ]
  where
    set = take 12 $ cycle $ reverse $ rotateList 2 $ brewerSet PuBu 4

dividedRing :: Double -> Double -> [(Kolor, String)] -> Diagram B
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
    segment :: ((Kolor, String), Int) -> Diagram B
    segment ((c, s), i) =
      (text s # translateX middle # rotate (θ i <> dθ))
      <> (annularWedge outer inner (rotate (θ i) xDir) dθ2 # fc c)

example :: Diagram B
example = (chart <> labels) # lw 0.1
  where
    chart :: Diagram B
    chart = mconcat $ mkRing <$> zip colorRows [0..]
    labels :: Diagram B
    labels = dividedRing (radius) (radius - 8) (zip (repeat white) modes)
    modes :: [String]
    modes = reverse $ take 84 $ cycle ["Lydian", "Ionian", "Mixolydian", "Dorian", "Aeolian", "Phygian", "Locrian"]
    columns :: [[String]]
    columns = [ note <$> modeNotes m n | n <- normalNotes, m <- reverse modeOrder ]
    rows :: [[String]]
    rows = transpose columns
    colorRows :: [[(Kolor, String)]]
    colorRows = [ (zip (rotateList k colors) row) | (k, row) <- zip [7, 5, 3, 1, 6, 4, 2] rows ]
    dr :: Double
    dr = 2
    radius :: Double
    radius = 35
    mkRing :: ([(Kolor, String)], Int) -> Diagram B
    mkRing (colorRow, i) = dividedRing (radius + (i'+1) * dr) (radius + i' * dr) colorRow
      where
        i' = fromIntegral i

main :: IO ()
main = mainWith example
