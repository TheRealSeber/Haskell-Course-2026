{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Types
import Parser
import Graph
import Evaluator
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do

  describe "Parser" $ do

    it "parses a literal number cell" $
      parseSheet "sheet { A1 = 42; }"
        `shouldBe` Right (Sheet (Map.fromList [(("A",1), Lit (NumV 42))]))

    it "parses a string literal cell" $
      parseSheet "sheet { A1 = \"hello\"; }"
        `shouldBe` Right (Sheet (Map.fromList [(("A",1), Lit (StrV "hello"))]))

    it "parses a reference formula" $
      parseSheet "sheet { A1 = 1; A2 = A1; }"
        `shouldBe` Right (Sheet (Map.fromList
          [ (("A",1), Lit  (NumV 1))
          , (("A",2), Form (Ref ("A",1)))
          ]))

    it "parses an addition formula" $
      parseSheet "sheet { A1 = 1; A2 = 2; A3 = A1 + A2; }"
        `shouldBe` Right (Sheet (Map.fromList
          [ (("A",1), Lit  (NumV 1))
          , (("A",2), Lit  (NumV 2))
          , (("A",3), Form (BinOp Add (Ref ("A",1)) (Ref ("A",2))))
          ]))

    it "respects operator precedence (* before +)" $
      parseSheet "sheet { A1 = 2 + 3 * 4; }"
        `shouldBe` Right (Sheet (Map.fromList
          [ (("A",1), Form (BinOp Add
              (LitE (NumV 2))
              (BinOp Mul (LitE (NumV 3)) (LitE (NumV 4)))))
          ]))

    it "parses a SUM range" $
      parseSheet "sheet { A1 = 1; A2 = SUM(A1:A1); }"
        `shouldBe` Right (Sheet (Map.fromList
          [ (("A",1), Lit  (NumV 1))
          , (("A",2), Form (RangeE SumR ("A",1) ("A",1)))
          ]))

    it "ignores line comments" $
      parseSheet "sheet { -- a comment\n  A1 = 1; }"
        `shouldBe` Right (Sheet (Map.fromList [(("A",1), Lit (NumV 1))]))

    it "returns Left on bad input" $
      case parseSheet "not a sheet" of
        Left _  -> True `shouldBe` True
        Right _ -> expectationFailure "expected parse error"

    it "returns Left on missing semicolon" $
      case parseSheet "sheet { A1 = 1 }" of
        Left _  -> True `shouldBe` True
        Right _ -> expectationFailure "expected parse error"

  describe "Graph: deps" $ do

    it "returns [] for a literal" $
      deps (LitE (NumV 1)) `shouldBe` []

    it "returns the addr for Ref" $
      deps (Ref ("A",1)) `shouldBe` [("A",1)]

    it "returns all refs in BinOp" $
      deps (BinOp Add (Ref ("A",1)) (Ref ("B",2)))
        `shouldBe` [("A",1), ("B",2)]

    it "expands RangeE to all addresses" $
      deps (RangeE SumR ("A",1) ("A",3))
        `shouldBe` [("A",1), ("A",2), ("A",3)]

  describe "Graph: topoSort" $ do

    it "sorts a simple chain A1 <- A2 <- A3" $
      let graph = Map.fromList
            [ (("A",1), [])
            , (("A",2), [("A",1)])
            , (("A",3), [("A",2)])
            ]
      in topoSort graph `shouldBe` Right [("A",1), ("A",2), ("A",3)]

    it "detects self-cycle A1 = A1" $
      let graph = Map.fromList [(("A",1), [("A",1)])]
      in case topoSort graph of
           Left _  -> True `shouldBe` True
           Right _ -> expectationFailure "expected cycle detection"

    it "detects two-cell cycle" $
      let graph = Map.fromList
            [ (("A",1), [("A",2)])
            , (("A",2), [("A",1)])
            ]
      in case topoSort graph of
           Left _  -> True `shouldBe` True
           Right _ -> expectationFailure "expected cycle detection"

    it "independent cell is not reported as cyclic" $
      let graph = Map.fromList
            [ (("A",1), [("A",1)])
            , (("B",1), [])
            ]
      in case topoSort graph of
           Left cyclic -> cyclic `shouldNotContain` [("B",1)]
           Right _     -> expectationFailure "expected cycle detection"

  describe "Evaluator: evalExpr" $ do

    let env = Map.fromList
          [ (("A",1), NumV 10)
          , (("A",2), NumV 5)
          , (("A",3), ErrV "cycle")
          ]

    it "evaluates a literal" $
      evalExpr env (LitE (NumV 42)) `shouldBe` Right (NumV 42)

    it "looks up an existing reference" $
      evalExpr env (Ref ("A",1)) `shouldBe` Right (NumV 10)

    it "returns Left for a missing reference" $
      case evalExpr env (Ref ("Z",9)) of
        Left _  -> True `shouldBe` True
        Right _ -> expectationFailure "expected error for missing ref"

    it "propagates ErrV through Ref" $
      case evalExpr env (Ref ("A",3)) of
        Left _  -> True `shouldBe` True
        Right _ -> expectationFailure "expected propagated ErrV"

    it "adds two numbers" $
      evalExpr env (BinOp Add (Ref ("A",1)) (Ref ("A",2)))
        `shouldBe` Right (NumV 15)

    it "subtracts" $
      evalExpr env (BinOp Sub (Ref ("A",1)) (Ref ("A",2)))
        `shouldBe` Right (NumV 5)

    it "multiplies" $
      evalExpr env (BinOp Mul (LitE (NumV 3)) (LitE (NumV 4)))
        `shouldBe` Right (NumV 12)

    it "divides" $
      evalExpr env (BinOp Div (LitE (NumV 9)) (LitE (NumV 3)))
        `shouldBe` Right (NumV 3)

    it "returns Left for division by zero" $
      case evalExpr env (BinOp Div (LitE (NumV 1)) (LitE (NumV 0))) of
        Left _  -> True `shouldBe` True
        Right _ -> expectationFailure "expected division by zero error"

    it "returns Left for type error" $
      case evalExpr env (BinOp Add (LitE (StrV "a")) (LitE (NumV 1))) of
        Left _  -> True `shouldBe` True
        Right _ -> expectationFailure "expected type error"

    it "evaluates SUM range (A1+A2 = 15)" $
      evalExpr env (RangeE SumR ("A",1) ("A",2))
        `shouldBe` Right (NumV 15)

  describe "evalSheet end-to-end" $ do

    it "A3 = A1 + A2 computes correctly" $ do
      let Right sheet = parseSheet "sheet { A1 = 10; A2 = 20; A3 = A1 + A2; }"
      Map.lookup ("A",3) (evalSheet sheet) `shouldBe` Just (NumV 30)

    it "SUM(A1:A3) sums the range" $ do
      let Right sheet = parseSheet "sheet { A1 = 1; A2 = 2; A3 = 3; A4 = SUM(A1:A3); }"
      Map.lookup ("A",4) (evalSheet sheet) `shouldBe` Just (NumV 6)

    it "AVG(A1:A2) averages correctly" $ do
      let Right sheet = parseSheet "sheet { A1 = 10; A2 = 20; A3 = AVG(A1:A2); }"
      Map.lookup ("A",3) (evalSheet sheet) `shouldBe` Just (NumV 15)

    it "division by zero produces ErrV" $ do
      let Right sheet = parseSheet "sheet { A1 = 1; A2 = 0; A3 = A1 / A2; }"
      Map.lookup ("A",3) (evalSheet sheet) `shouldBe` Just (ErrV "division by zero")

    it "self-cycle does not hang, produces ErrV cycle" $ do
      let Right sheet = parseSheet "sheet { A1 = A1; }"
      Map.lookup ("A",1) (evalSheet sheet) `shouldBe` Just (ErrV "cycle")

    it "two-cell cycle does not hang" $ do
      let Right sheet = parseSheet "sheet { A1 = A2; A2 = A1; }"
          result      = evalSheet sheet
      Map.lookup ("A",1) result `shouldSatisfy` (== Just (ErrV "cycle"))
      Map.lookup ("A",2) result `shouldSatisfy` (== Just (ErrV "cycle"))

    it "ErrV propagates to dependent cells" $ do
      let Right sheet = parseSheet "sheet { A1 = A1; A2 = A1 + 1; }"
      Map.lookup ("A",2) (evalSheet sheet) `shouldBe` Just (ErrV "cycle")

    it "multi-level chain: B1 = A3 * 2, A3 = A1 + A2" $ do
      let Right sheet = parseSheet
            "sheet { A1 = 5; A2 = 5; A3 = A1 + A2; B1 = A3 * 2; }"
      Map.lookup ("B",1) (evalSheet sheet) `shouldBe` Just (NumV 20)

  describe "Property tests" $ do

    it "literal cell always evaluates to its value" $
      property $ \(n :: Int) ->
        let i     = abs n `mod` 99 + 1
            sheet = Sheet (Map.fromList [(("A",i), Lit (NumV (fromIntegral i)))])
        in Map.lookup ("A",i) (evalSheet sheet) == Just (NumV (fromIntegral i))

    it "evalSheet is deterministic" $
      let sheet = Sheet (Map.fromList
            [ (("A",1), Lit (NumV 5))
            , (("A",2), Form (BinOp Add (Ref ("A",1)) (LitE (NumV 3))))
            ])
      in evalSheet sheet `shouldBe` evalSheet sheet
