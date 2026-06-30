module UiLayoutTerminal
  ( renderTerminal
  ) where

import Data.Char (toUpper)
import UiLayoutEngine (resolveWindow)
import UiLayoutTypes

renderTerminal :: Window -> String
renderTerminal win@(Window w h _) =
  unlines (drawAll (resolveWindow win) blank)
  where
    canvasW = max 1 (min 80 w)
    canvasH = max 1 (min 24 h)
    blank = replicate canvasH (replicate canvasW ' ')

    scaleX x = min (canvasW - 1) (x * canvasW `div` max 1 w)
    scaleY y = min (canvasH - 1) (y * canvasH `div` max 1 h)

    drawAll node canvas =
      foldl (flip drawAll) (drawBox node canvas) (rChildren node)

    drawBox node canvas =
      let x1 = scaleX (rx node)
          y1 = scaleY (ry node)
          x2 = scaleX (rx node + max 0 (rw node - 1))
          y2 = scaleY (ry node + max 0 (rh node - 1))
          ch = colorChar (rColor node)
       in fillRect x1 y1 x2 y2 ch canvas

colorChar :: Maybe String -> Char
colorChar Nothing = '.'
colorChar (Just []) = '.'
colorChar (Just (c : _)) = toUpper c

fillRect :: Int -> Int -> Int -> Int -> Char -> [String] -> [String]
fillRect x1 y1 x2 y2 ch canvas =
  [ if y >= y1 && y <= y2 then fillLine lineText else lineText
  | (y, lineText) <- zip [0 ..] canvas
  ]
  where
    fillLine lineText =
      [ if x >= x1 && x <= x2 then ch else old
      | (x, old) <- zip [0 ..] lineText
      ]
