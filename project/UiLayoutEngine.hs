module UiLayoutEngine
  ( resolveWindow
  , resolveLayout
  , layoutSummary
  ) where

import UiLayoutTypes

resolveWindow :: Window -> Resolved
resolveWindow (Window w h layout) = resolveLayout 0 0 w h layout

resolveLayout :: Int -> Int -> Int -> Int -> Layout -> Resolved
resolveLayout x y w h (Layout props children) =
  Resolved x y w h (propDir props) (propColor props) (resolveChildren (propDir props) x y w h children)

resolveChildren :: Direction -> Int -> Int -> Int -> Int -> [Layout] -> [Resolved]
resolveChildren direction x y w h children =
  case direction of
    Row -> goRow x w children
    Col -> goCol y h children
  where
    goRow _ _ [] = []
    goRow cursor remaining (child@(Layout props _) : rest) =
      let wantedW = clamp 0 remaining (resolveSize (propWidth props) w)
          wantedH = clamp 0 h (resolveSize (propHeight props) h)
          resolved = resolveLayout cursor y wantedW wantedH child
       in resolved : goRow (cursor + wantedW) (remaining - wantedW) rest

    goCol _ _ [] = []
    goCol cursor remaining (child@(Layout props _) : rest) =
      let wantedW = clamp 0 w (resolveSize (propWidth props) w)
          wantedH = clamp 0 remaining (resolveSize (propHeight props) h)
          resolved = resolveLayout x cursor wantedW wantedH child
       in resolved : goCol (cursor + wantedH) (remaining - wantedH) rest

resolveSize :: Size -> Int -> Int
resolveSize size parent =
  case size of
    Px n -> n
    Pct p -> round (p * fromIntegral parent)
    Fill -> parent

clamp :: Int -> Int -> Int -> Int
clamp lo hi value = max lo (min hi value)

layoutSummary :: Resolved -> String
layoutSummary = unlines . go 0
  where
    go depth node =
      let indent = replicate (depth * 2) ' '
          line =
            indent
              ++ "box x="
              ++ show (rx node)
              ++ " y="
              ++ show (ry node)
              ++ " w="
              ++ show (rw node)
              ++ " h="
              ++ show (rh node)
              ++ maybe "" (\c -> " color=" ++ c) (rColor node)
       in line : concatMap (go (depth + 1)) (rChildren node)
