module Parser
  ( parseSheet
  ) where

import Types
import qualified Data.Map as Map
import Data.Char    (isAlpha, isDigit, isSpace)
import Data.List    (isPrefixOf)

skipWS :: String -> String
skipWS []             = []
skipWS ('-':'-':rest) = skipWS (dropWhile (/= '\n') rest)
skipWS (c:cs)
  | isSpace c = skipWS cs
  | otherwise = c : cs

expect :: String -> String -> Either String String
expect tok input
  | tok `isPrefixOf` input = Right (drop (length tok) input)
  | otherwise = Left ("expected '" ++ tok ++ "', got: "
                      ++ take (max 10 (length tok)) input)

parseSheet :: String -> Either String Sheet
parseSheet input = do
  rest1 <- expect "sheet" (skipWS input)
  rest2 <- expect "{" (skipWS rest1)
  (pairs, rest3) <- parseCells (skipWS rest2) []
  rest4 <- expect "}" (skipWS rest3)
  if null (skipWS rest4)
    then Right (Sheet (Map.fromList pairs))
    else Left ("trailing input: " ++ take 20 (skipWS rest4))

parseCells :: String -> [(Addr, Content)]
           -> Either String ([(Addr, Content)], String)
parseCells input acc =
  case skipWS input of
    ('}':_) -> Right (reverse acc, input)
    []      -> Left "unexpected end of input, expected '}'"
    s       -> do
      (pair, rest) <- parseCell s
      parseCells rest (pair : acc)

parseCell :: String -> Either String ((Addr, Content), String)
parseCell input = do
  (addr,    rest1) <- parseAddr    (skipWS input)
  rest2            <- expect "="   (skipWS rest1)
  (content, rest3) <- parseContent (skipWS rest2)
  rest4            <- expect ";"   (skipWS rest3)
  Right ((addr, content), rest4)

parseContent :: String -> Either String (Content, String)
parseContent input = do
  (expr, rest) <- parseExpr input
  case expr of
    LitE v -> Right (Lit v,  rest)
    e      -> Right (Form e, rest)

-- Additive level: left-associative + and -
parseExpr :: String -> Either String (Expr, String)
parseExpr input = do
  (left, rest) <- parseTerm input
  parseExprTail left (skipWS rest)

parseExprTail :: Expr -> String -> Either String (Expr, String)
parseExprTail left ('+':rest) = do
  (right, rest2) <- parseTerm (skipWS rest)
  parseExprTail (BinOp Add left right) (skipWS rest2)
parseExprTail left ('-':rest) = do
  (right, rest2) <- parseTerm (skipWS rest)
  parseExprTail (BinOp Sub left right) (skipWS rest2)
parseExprTail left rest = Right (left, rest)

-- Multiplicative level: left-associative * and /
parseTerm :: String -> Either String (Expr, String)
parseTerm input = do
  (left, rest) <- parsePrimary input
  parseTermTail left (skipWS rest)

parseTermTail :: Expr -> String -> Either String (Expr, String)
parseTermTail left ('*':rest) = do
  (right, rest2) <- parsePrimary (skipWS rest)
  parseTermTail (BinOp Mul left right) (skipWS rest2)
parseTermTail left ('/':rest) = do
  (right, rest2) <- parsePrimary (skipWS rest)
  parseTermTail (BinOp Div left right) (skipWS rest2)
parseTermTail left rest = Right (left, rest)

-- Atoms: parens, strings, functions/refs/booleans, numbers
parsePrimary :: String -> Either String (Expr, String)
parsePrimary [] = Left "unexpected end of input"
parsePrimary ('(':rest) = do
  (expr, rest2) <- parseExpr (skipWS rest)
  rest3         <- expect ")" (skipWS rest2)
  Right (expr, rest3)
parsePrimary ('"':rest) =
  let str  = takeWhile (/= '"') rest
      rest2 = drop (length str + 1) rest
  in Right (LitE (StrV str), rest2)
parsePrimary input@(c:_)
  | isAlpha c =
      let word  = takeWhile isAlpha input
          after = drop (length word) input
      in case after of
           ('(':_) -> case word of
             "SUM" -> parseRangeOp SumR after
             "AVG" -> parseRangeOp AvgR after
             _     -> Left ("unknown function: " ++ word)
           _ ->
             let digits   = takeWhile isDigit after
                 afterNum = drop (length digits) after
             in if null digits
                then case word of
                  "True"  -> Right (LitE (BoolV True),  after)
                  "False" -> Right (LitE (BoolV False), after)
                  _       -> Left ("expected digit after '" ++ word ++ "'")
                else Right (Ref (word, read digits), afterNum)
  | isDigit c || c == '.' =
      let numStr = takeWhile (\x -> isDigit x || x == '.') input
      in Right (LitE (NumV (read numStr)), drop (length numStr) input)
  | otherwise = Left ("unexpected character: " ++ [c])

parseRangeOp :: RangeOp -> String -> Either String (Expr, String)
parseRangeOp op input = do
  rest1          <- expect "(" input
  (addr1, rest2) <- parseAddr (skipWS rest1)
  rest3          <- expect ":" (skipWS rest2)
  (addr2, rest4) <- parseAddr (skipWS rest3)
  rest5          <- expect ")" (skipWS rest4)
  Right (RangeE op addr1 addr2, rest5)

parseAddr :: String -> Either String (Addr, String)
parseAddr input =
  let col    = takeWhile isAlpha input
      rest   = drop (length col) input
      digits = takeWhile isDigit rest
      rest2  = drop (length digits) rest
  in if null col
     then Left ("expected cell address, got: " ++ take 10 input)
     else if null digits
          then Left ("expected row number after '" ++ col ++ "'")
          else Right ((col, read digits), rest2)
