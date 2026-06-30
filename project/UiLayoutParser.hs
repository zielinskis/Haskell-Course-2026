module UiLayoutParser
  ( parseWindow
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import UiLayoutTypes

data Token = Token String Int Int
  deriving (Eq, Show)

parseWindow :: String -> Either String Window
parseWindow source = do
  tokens <- tokenize 1 1 source
  (win, rest) <- parseWin tokens
  case rest of
    [] -> Right win
    token : _ -> Left ("Unexpected token " ++ showToken token)

parseWin :: [Token] -> Either String (Window, [Token])
parseWin tokens0 = do
  tokens1 <- expect "window" tokens0
  (widthText, tokens2) <- takeToken "window width" tokens1
  tokens3 <- expect "x" tokens2
  (heightText, tokens4) <- takeToken "window height" tokens3
  tokens5 <- expect "{" tokens4
  (children, tokens6) <- parseLayouts tokens5
  tokens7 <- expect "}" tokens6
  width <- readInt widthText
  height <- readInt heightText
  case children of
    [] -> Left "Window must contain at least one layout"
    [layout] -> Right (Window width height layout, tokens7)
    layouts -> Right (Window width height (col layouts), tokens7)

parseLayouts :: [Token] -> Either String ([Layout], [Token])
parseLayouts tokens =
  case tokens of
    Token "}" _ _ : _ -> Right ([], tokens)
    [] -> Left "Expected layout or }"
    _ -> do
      (firstLayout, afterFirst) <- parseLayout tokens
      (otherLayouts, afterAll) <- parseLayouts afterFirst
      Right (firstLayout : otherLayouts, afterAll)

parseLayout :: [Token] -> Either String (Layout, [Token])
parseLayout tokens =
  case tokens of
    Token "row" _ _ : rest -> parseContainer Row rest
    Token "col" _ _ : rest -> parseContainer Col rest
    Token "box" _ _ : rest -> parseBox rest
    token : _ -> Left ("Expected row, col, or box, got " ++ showToken token)
    [] -> Left "Expected row, col, or box"

parseContainer :: Direction -> [Token] -> Either String (Layout, [Token])
parseContainer direction tokens0 = do
  tokens1 <- expect "{" tokens0
  (children, tokens2) <- parseLayouts tokens1
  tokens3 <- expect "}" tokens2
  let props = defaultProps {propDir = direction}
  Right (Layout props children, tokens3)

parseBox :: [Token] -> Either String (Layout, [Token])
parseBox tokens0 = do
  tokens1 <- expect "{" tokens0
  (props, children, tokens2) <- parseBoxBody defaultProps [] tokens1
  tokens3 <- expect "}" tokens2
  Right (Layout props children, tokens3)

parseBoxBody :: Props -> [Layout] -> [Token] -> Either String (Props, [Layout], [Token])
parseBoxBody props children tokens =
  case tokens of
    Token "}" _ _ : _ -> Right (props, children, tokens)
    Token "row" _ _ : _ -> parseChild
    Token "col" _ _ : _ -> parseChild
    Token "box" _ _ : _ -> parseChild
    Token name _ _ : rest -> do
      afterColon <- expect ":" rest
      (newProps, afterValue) <- parseProperty props name afterColon
      parseBoxBody newProps children (skipSeparator afterValue)
    [] -> Left "Expected property or } in box"
  where
    parseChild = do
      (child, rest) <- parseLayout tokens
      parseBoxBody props (children ++ [child]) rest

parseProperty :: Props -> String -> [Token] -> Either String (Props, [Token])
parseProperty props name tokens =
  case name of
    "width" -> do
      (size, rest) <- parseSize tokens
      Right (props {propWidth = size}, rest)
    "height" -> do
      (size, rest) <- parseSize tokens
      Right (props {propHeight = size}, rest)
    "dir" -> do
      (direction, rest) <- parseDirection tokens
      Right (props {propDir = direction}, rest)
    "direction" -> do
      (direction, rest) <- parseDirection tokens
      Right (props {propDir = direction}, rest)
    "color" -> do
      (colorName, rest) <- takeToken "color" tokens
      let color = if colorName == "none" then Nothing else Just colorName
      Right (props {propColor = color}, rest)
    _ -> Left ("Unknown property " ++ name)

parseSize :: [Token] -> Either String (Size, [Token])
parseSize tokens =
  case tokens of
    Token "fill" _ _ : rest -> Right (Fill, rest)
    Token numberText _ _ : Token "%" _ _ : rest -> do
      value <- readDouble numberText
      Right (Pct (value / 100), rest)
    Token numberText _ _ : Token "px" _ _ : rest -> do
      value <- readDouble numberText
      Right (Px (round value), rest)
    Token numberText _ _ : rest -> do
      value <- readDouble numberText
      Right (Px (round value), rest)
    [] -> Left "Expected size"

parseDirection :: [Token] -> Either String (Direction, [Token])
parseDirection tokens =
  case tokens of
    Token "row" _ _ : rest -> Right (Row, rest)
    Token "col" _ _ : rest -> Right (Col, rest)
    Token "column" _ _ : rest -> Right (Col, rest)
    token : _ -> Left ("Expected row or col, got " ++ showToken token)
    [] -> Left "Expected direction"

expect :: String -> [Token] -> Either String [Token]
expect wanted tokens =
  case tokens of
    Token actual _ _ : rest | actual == wanted -> Right rest
    token : _ -> Left ("Expected " ++ wanted ++ ", got " ++ showToken token)
    [] -> Left ("Expected " ++ wanted)

takeToken :: String -> [Token] -> Either String (String, [Token])
takeToken what tokens =
  case tokens of
    Token text _ _ : rest -> Right (text, rest)
    [] -> Left ("Expected " ++ what)

skipSeparator :: [Token] -> [Token]
skipSeparator (Token ";" _ _ : rest) = rest
skipSeparator (Token "," _ _ : rest) = rest
skipSeparator tokens = tokens

readInt :: String -> Either String Int
readInt text =
  case reads text of
    [(n, "")] -> Right n
    _ -> Left ("Expected integer, got " ++ text)

readDouble :: String -> Either String Double
readDouble text =
  case reads text of
    [(n, "")] -> Right n
    _ -> Left ("Expected number, got " ++ text)

showToken :: Token -> String
showToken (Token text line colNum) =
  show text ++ " at line " ++ show line ++ ", column " ++ show colNum

tokenize :: Int -> Int -> String -> Either String [Token]
tokenize _ _ [] = Right []
tokenize line colNum input@(c : cs)
  | isSpace c = tokenize nextLine nextCol cs
  | c == '"' = tokenizeString line colNum "" cs
  | c `elem` "{}:;,%x" && c /= 'x' = addToken [c] line colNum (tokenize line (colNum + 1) cs)
  | isDigit c = tokenizeNumber line colNum input
  | isAlpha c = tokenizeWord line colNum input
  | otherwise = Left ("Unexpected character " ++ show c ++ " at line " ++ show line ++ ", column " ++ show colNum)
  where
    nextLine = if c == '\n' then line + 1 else line
    nextCol = if c == '\n' then 1 else colNum + 1

tokenizeWord :: Int -> Int -> String -> Either String [Token]
tokenizeWord line colNum input =
  let (wordText, rest) = span (\x -> isAlphaNum x || x == '-' || x == '_') input
   in addToken wordText line colNum (tokenize line (colNum + length wordText) rest)

tokenizeNumber :: Int -> Int -> String -> Either String [Token]
tokenizeNumber line colNum input =
  let (numberText, rest) = span (\x -> isDigit x || x == '.') input
   in addToken numberText line colNum (tokenize line (colNum + length numberText) rest)

tokenizeString :: Int -> Int -> String -> String -> Either String [Token]
tokenizeString _ _ _ [] = Left "Unclosed string"
tokenizeString line colNum acc (c : cs)
  | c == '"' = addToken (reverse acc) line colNum (tokenize line (colNum + length acc + 2) cs)
  | otherwise = tokenizeString line colNum (c : acc) cs


addToken :: String -> Int -> Int -> Either String [Token] -> Either String [Token]
addToken text line colNum rest =
  case rest of
    Left err -> Left err
    Right tokens -> Right (Token text line colNum : tokens)

dropLine :: String -> String
dropLine = dropWhile (/= '\n')
