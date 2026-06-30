module Main (main) where

import Lib
import Types
import qualified Data.Map as Map

demo :: String
demo = unlines
  [ "sheet {"
  , "  A1 = 10;"
  , "  A2 = 20;"
  , "  A3 = A1 + A2;"
  , "  A4 = SUM(A1:A3);"
  , "  B1 = A3 * 2;"
  , "}"
  ]

main :: IO ()
main = do
  putStrLn "=== SpreadsheetLang Demo ==="
  putStrLn demo
  let result = runSheet demo
  putStrLn "Results:"
  mapM_ (\(addr, val) ->
    putStrLn ("  " ++ showAddr addr ++ " = " ++ show val))
    (Map.toAscList result)
