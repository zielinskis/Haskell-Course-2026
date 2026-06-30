module UiLayoutTests
  ( runTests
  ) where

import System.Exit (exitFailure)
import UiLayoutEngine (resolveWindow)
import UiLayoutParser (parseWindow)
import UiLayoutTerminal (renderTerminal)
import UiLayoutTypes

data Test = Test String (Either String ())

sampleProgram :: String
sampleProgram =
  unlines
    [ "window 800 x 600 {"
    , "  row {"
    , "    box { width: 20%; height: 100%; color: red }"
    , "    box { width: 80%; height: 100%; color: blue }"
    , "  }"
    , "}"
    ]

runTests :: IO ()
runTests = do
  let results = tests
      failures = [(name, msg) | Test name (Left msg) <- results]
  mapM_ printResult results
  if null failures
    then putStrLn ("All " ++ show (length results) ++ " tests passed.")
    else do
      putStrLn (show (length failures) ++ " test(s) failed.")
      exitFailure

printResult :: Test -> IO ()
printResult (Test name result) =
  putStrLn $
    case result of
      Right () -> "[pass] " ++ name
      Left msg -> "[fail] " ++ name ++ ": " ++ msg

tests :: [Test]
tests =
  [ testParserExample
  , testSingleBoxFill
  , testRowHalves
  , testOverflowClamps
  , testNestedLayout
  , testTerminalRender
  , testSimpleInvariants
  ]

testParserExample :: Test
testParserExample =
  Test "parser accepts the example language" $ do
    win <- parseForTest sampleProgram
    assertEqual "window width" 800 (windowWidth win)
    assertEqual "window height" 600 (windowHeight win)

testSingleBoxFill :: Test
testSingleBoxFill =
  Test "a 100% box fills its parent" $ do
    let source =
          unlines
            [ "window 300 x 200 {"
            , "  box { width: 100%; height: 100%; color: green }"
            , "}"
            ]
    resolved <- resolveForTest source
    assertEqual "root rectangle" (0, 0, 300, 200) (rect resolved)

testRowHalves :: Test
testRowHalves =
  Test "two 50% row children split the parent" $ do
    let source =
          unlines
            [ "window 100 x 40 {"
            , "  row {"
            , "    box { width: 50%; height: 100% }"
            , "    box { width: 50%; height: 100% }"
            , "  }"
            , "}"
            ]
    resolved <- resolveForTest source
    assertEqual "children rectangles" [(0, 0, 50, 40), (50, 0, 50, 40)] (map rect (rChildren resolved))

testOverflowClamps :: Test
testOverflowClamps =
  Test "overflow is clipped to remaining space" $ do
    let source =
          unlines
            [ "window 100 x 20 {"
            , "  row {"
            , "    box { width: 80px; height: 100% }"
            , "    box { width: 80px; height: 100% }"
            , "  }"
            , "}"
            ]
    resolved <- resolveForTest source
    assertEqual "clamped row children" [(0, 0, 80, 20), (80, 0, 20, 20)] (map rect (rChildren resolved))

testNestedLayout :: Test
testNestedLayout =
  Test "nested rows and columns resolve deterministically" $ do
    let source =
          unlines
            [ "window 200 x 100 {"
            , "  row {"
            , "    col {"
            , "      box { width: 100%; height: 25px }"
            , "      box { width: 100%; height: 75px }"
            , "    }"
            , "    box { width: 50px; height: 100% }"
            , "  }"
            , "}"
            ]
    resolved <- resolveForTest source
    let firstChild = head (rChildren resolved)
    assertEqual "outer row" [(0, 0, 200, 100), (200, 0, 0, 100)] (map rect (rChildren resolved))
    assertEqual "inner column" [(0, 0, 200, 25), (0, 25, 200, 75)] (map rect (rChildren firstChild))


testTerminalRender :: Test
testTerminalRender =
  Test "terminal renderer draws colored boxes" $ do
    win <- parseForTest sampleProgram
    let picture = renderTerminal win
    assertEqual "red area appears" True ('R' `elem` picture)
    assertEqual "blue area appears" True ('B' `elem` picture)

testSimpleInvariants :: Test
testSimpleInvariants =
  Test "children stay inside their parents" $ do
    resolved <- resolveForTest sampleProgram
    assertEqual "containment" True (contained resolved)
    assertEqual "axis sum" True (axisSumsFit resolved)

contained :: Resolved -> Bool
contained parent =
  all (inside parent) (rChildren parent) && all contained (rChildren parent)

inside :: Resolved -> Resolved -> Bool
inside parent child =
  rx child >= rx parent
    && ry child >= ry parent
    && rx child + rw child <= rx parent + rw parent
    && ry child + rh child <= ry parent + rh parent

axisSumsFit :: Resolved -> Bool
axisSumsFit node =
  currentFits && all axisSumsFit (rChildren node)
  where
    currentFits =
      case rDir node of
        Row -> sum (map rw (rChildren node)) <= rw node
        Col -> sum (map rh (rChildren node)) <= rh node

rect :: Resolved -> (Int, Int, Int, Int)
rect node = (rx node, ry node, rw node, rh node)

parseForTest :: String -> Either String Window
parseForTest = parseWindow

resolveForTest :: String -> Either String Resolved
resolveForTest source = resolveWindow <$> parseForTest source

assertEqual :: (Eq a, Show a) => String -> a -> a -> Either String ()
assertEqual label expected actual =
  if expected == actual
    then Right ()
    else Left (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
