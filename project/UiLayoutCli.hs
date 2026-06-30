module UiLayoutCli
  ( main
  ) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import UiLayoutEngine (layoutSummary, resolveWindow)
import UiLayoutParser (parseWindow)
import UiLayoutTerminal (renderTerminal)
import UiLayoutTests (runTests)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> runTests
    ["parse", input] -> readFile input >>= printParseResult
    ["layout", input] -> readFile input >>= runLayoutText
    ["render", input] -> readFile input >>= runTerminalRender
    _ -> do
      putStrLn "Unknown command."
      exitFailure

printParseResult :: String -> IO ()
printParseResult source =
  case parseWindow source of
    Left err -> print err >> exitFailure
    Right win -> print win

runLayoutText :: String -> IO ()
runLayoutText source =
  case parseWindow source of
    Left err -> print err >> exitFailure
    Right win -> putStr (layoutSummary (resolveWindow win))

runTerminalRender :: String -> IO ()
runTerminalRender source =
  case parseWindow source of
    Left err -> print err >> exitFailure
    Right win -> putStr (renderTerminal win)
