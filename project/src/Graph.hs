module Graph
  ( deps
  , expandRange
  , buildGraph
  , topoSort
  , topoSortFull
  ) where

import Types
import Data.Map  (Map)
import qualified Data.Map  as Map
import Data.Set  (Set)
import qualified Data.Set  as Set
import Control.Monad.State

deps :: Expr -> [Addr]
deps (Ref addr)        = [addr]
deps (LitE _)          = []
deps (BinOp _ l r)     = deps l ++ deps r
deps (RangeE _ a1 a2)  = expandRange a1 a2

expandRange :: Addr -> Addr -> [Addr]
expandRange (col, r1) (_, r2) =
  [(col, r) | r <- [min r1 r2 .. max r1 r2]]

buildGraph :: Sheet -> Map Addr [Addr]
buildGraph (Sheet m) = Map.map contentDeps m
  where
    contentDeps (Lit _)  = []
    contentDeps (Form e) = deps e

data Colour = White | Gray | Black deriving (Eq, Show)

data DFSState = DFSState
  { colours   :: Map Addr Colour
  , topoOrder :: [Addr]
  , cyclic    :: Set Addr
  } deriving (Show)

topoSort :: Map Addr [Addr] -> Either [Addr] [Addr]
topoSort graph =
  let (cyclicAddrs, order) = topoSortFull graph
  in if null cyclicAddrs
     then Right order
     else Left cyclicAddrs

topoSortFull :: Map Addr [Addr] -> ([Addr], [Addr])
topoSortFull graph =
  let allNodes = Map.keys graph
      initial  = DFSState
        { colours   = Map.fromList [(n, White) | n <- allNodes]
        , topoOrder = []
        , cyclic    = Set.empty
        }
      final = execState (mapM_ (visit graph) allNodes) initial
      cyclicSet = cyclic final
      safeOrder = filter (`Set.notMember` cyclicSet) (topoOrder final)
  in (Set.toList cyclicSet, safeOrder)

visit :: Map Addr [Addr] -> Addr -> State DFSState ()
visit graph node = do
  c <- gets (Map.findWithDefault White node . colours)
  case c of
    Black -> return ()
    Gray  -> do
      grayNodes <- gets (Map.keys . Map.filter (== Gray) . colours)
      modify (\s -> s { cyclic = Set.union (cyclic s) (Set.fromList grayNodes) })
    White -> do
      modify (\s -> s { colours = Map.insert node Gray (colours s) })
      let neighbours = Map.findWithDefault [] node graph
      mapM_ (visit graph) neighbours
      modify (\s -> s
        { colours   = Map.insert node Black (colours s)
        , topoOrder = topoOrder s ++ [node]
        })
