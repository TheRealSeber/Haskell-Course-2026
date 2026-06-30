module Lib
  ( runSheet
  , parseSheetOnly
  , evalParsedSheet
  ) where

import Types
import Parser    (parseSheet)
import Evaluator (evalSheet)
import Data.Map  (Map)

runSheet :: String -> Map Addr Value
runSheet input =
  case parseSheet input of
    Left _      -> mempty
    Right sheet -> evalSheet sheet

parseSheetOnly :: String -> Either String Sheet
parseSheetOnly = parseSheet

evalParsedSheet :: Sheet -> Map Addr Value
evalParsedSheet = evalSheet
