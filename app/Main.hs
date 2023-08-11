module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Tilings

example = drawTiling t3464 10 10 # lc white # lw thick
                                 # centerXY # pad 1.1

main = mainWith (example :: Diagram B)
