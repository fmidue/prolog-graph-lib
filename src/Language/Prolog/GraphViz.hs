{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts #-}
module Language.Prolog.GraphViz
  ( Graph
  , resolveTree, resolveFirstTree
  , resolveTreePreview, resolveTreeToFile
  , asInlineSvg
  ) where

import Control.Applicative (Alternative)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Fix
import Control.Concurrent (forkIO)
import Data.List (intercalate, intersperse)
import Data.Char (ord)
import qualified Data.ByteString as B (ByteString, hGetContents)

import Data.GraphViz
  ( GraphvizParams(..), GlobalAttributes(GraphAttrs), GraphvizCommand(Dot), GraphvizCanvas(Xlib), GraphvizOutput(..)
  , runGraphvizCommand, runGraphvizCanvas', graphElemsToDot, toLabel, nonClusteredParams, commandFor, graphvizWithHandle
  )
import Data.GraphViz.Attributes.Colors (Color(X11Color), WeightedColor(..))
import Data.GraphViz.Attributes.Colors.X11 (X11Color(..))
import Data.GraphViz.Attributes.HTML
import Data.GraphViz.Attributes.Complete (Attribute(Ordering,Shape,Color,Width,Regular), Shape(BoxShape), Order(OutEdges))

import qualified Data.Text.Lazy

import Language.Prolog

htmlStr :: String -> TextItem
htmlStr = Str . Data.Text.Lazy.pack

resolveTree :: Program -> [Goal] -> Either String ([Unifier], Graph)
resolveTree p q = runGraphGenT $ resolve_ p q

resolveFirstTree :: Program -> [Goal] -> Either String (Maybe Unifier, Graph)
resolveFirstTree p q = do
  (us,t) <- runGraphGenT $ resolveN_ 1 p q
  pure $ case us of
    [] -> (Nothing,t)
    (x:_) -> (Just x,t)

-- Graphical output of derivation tree
resolveTreePreview :: Program -> [Goal] -> IO ()
resolveTreePreview p q = do
  case resolveTree p q of
    Left e -> error e
    Right (_,g) -> preview g

resolveTreeToFile :: FilePath -> Program -> [Goal] -> IO FilePath
resolveTreeToFile path p q = do
 case resolveTree p q of
   Left e -> error e
   Right (_,graph) -> runGraphvizCommand Dot (toDot [] graph) Png path

preview :: Gr NodeLabel EdgeLabel -> IO ()
preview g = ign $ forkIO (ign $ runGraphvizCanvas' (toDot [] g) Xlib)
  where
    ign = (>> return ())

asInlineSvg :: Graph -> IO B.ByteString
asInlineSvg graph =
  graphvizWithHandle (commandFor dot) dot Svg B.hGetContents
  where
    dot = toDot [] graph

toDot attrs g = graphElemsToDot params (labNodes g) (labEdges g)
  where
    params = nonClusteredParams { fmtNode = \ (_,l) -> formatNode l
                                , fmtEdge = \ (_, _, l) -> formatEdge l
                                , globalAttributes = [GraphAttrs (Ordering OutEdges : attrs)] -- child nodes are drawn in edge-order
                                , isDirected = True
                                }

type Graph = Gr NodeLabel EdgeLabel
type NodeLabel = ((ProtoBranch, Branch), [Branch], CutFlag)
type EdgeLabel = (ProtoBranch, Branch)
type Branch = (Path, [(VariableName, Term)], [Term])
type Path = [Integer]
type ProtoBranch = Branch


data Gr a b = Gr [(Int,a)] [(Int, Int, b)] deriving Show

empty = Gr [] []
insEdge edge (Gr ns es) = Gr ns (es ++ [edge])
insNode node (Gr ns es) = Gr (ns ++ [node]) es
gelem n (Gr ns es) = n `elem` map fst ns
relabelNode f node (Gr ns es) = Gr (map (\(n,l) -> (n,if n == node then f l else l)) ns) es
labNodes (Gr ns _) = ns
labEdges (Gr _ es) = es


newtype GraphGenT m a = GraphGenT (StateT Graph m a) deriving (Monad, Functor, MonadFix, MonadPlus, Applicative, MonadError e, MonadState Graph, MonadTrans, Alternative)

runGraphGenT :: GraphGenT m a -> m (a, Graph)
runGraphGenT  (GraphGenT st) = runStateT st empty
execGraphGenT :: Monad m => GraphGenT m a -> m Graph
execGraphGenT (GraphGenT st) = execStateT st empty


instance Monad m => MonadGraphGen (GraphGenT m) where

   createConnections currentBranch@(path,_,_) protoBranches branches = do
      let current = hash path
      -- Ensure node is present (FIXME Why do we do this?)
      let protoBranch = error "Unknown protobranch accessed during graph generation"
      let label = ((protoBranch, currentBranch), branches, WasNotCut)
      modify $ \graph ->
          if gelem current graph
             then relabelNode (const label) current graph
             else insNode (current, label) graph
      -- Create nodes and edges to them
      forM_ (zip protoBranches branches) $ \x@(_,(pathOfTarget,_,_))-> do
        let new = hash pathOfTarget
        modify $ insNode (new, (x, [], WasNotCut))
        modify $ insEdge (current, new, x)

   markSolution usf = do
      return ()

   markCutBranches stackPrefix = do
      forM_ stackPrefix $ \((path_,u_,gs_),alts_) -> do
         forM_ alts_ $ \(pathOfChild,_,_) -> do
            let child = hash pathOfChild
            modifyLabel child $ \(t,b,_) -> (t,b,WasCut)

data CutFlag = WasNotCut | WasCut



formatNode :: NodeLabel -> [Data.GraphViz.Attributes.Complete.Attribute]
formatNode ((_,(_,u',[])), _, WasNotCut) = -- Success
  Shape BoxShape :
  case filterOriginal u' of
    [] -> [ toLabel ("" :: String)
          , Data.GraphViz.Attributes.Complete.Width 0.2
          , Regular True
          , Data.GraphViz.Attributes.Complete.Color [(WC (X11Color Green) Nothing)]
          ]
    uf -> [ toLabel $ colorize Green $ htmlUnifier uf
          , Data.GraphViz.Attributes.Complete.Color [(WC (X11Color Green) Nothing)]
          ]
formatNode ((_,(_,_,gs')), [], WasNotCut) = -- Failure
    [ toLabel $ colorize Red [htmlGoals gs']
    , Data.GraphViz.Attributes.Complete.Color [(WC (X11Color Red) Nothing)]
    ]
formatNode ((_,(_,_,gs')), _, WasNotCut) =
    [ toLabel [htmlGoals gs'] ]
formatNode ((_,(_,u',[])), _, WasCut) = -- Cut with Succees
  Shape BoxShape :
  case filterOriginal u' of
    [] -> [ toLabel ("" :: String)
          , Data.GraphViz.Attributes.Complete.Width 0.2
          , Regular True
          , Data.GraphViz.Attributes.Complete.Color [(WC (X11Color Gray) Nothing)]
          ]
    uf -> [ toLabel $ colorize Gray $ htmlUnifier uf
          , Data.GraphViz.Attributes.Complete.Color [(WC (X11Color Gray) Nothing)]
          ]
formatNode ((_,(_,_,gs')), _, WasCut) = -- Cut
    [ toLabel $ colorize Gray [htmlGoals gs']
    , Data.GraphViz.Attributes.Complete.Color [(WC (X11Color Gray) Nothing)]
    ]


formatEdge :: EdgeLabel -> [Data.GraphViz.Attributes.Complete.Attribute]
formatEdge ((_,u ,_),_)  =
    [ toLabel [Font [PointSize 8] $ htmlUnifier $ simplify u] ]

simplify = ([] +++)

htmlGoals = htmlStr . intercalate "," . map show

htmlUnifier [] = [htmlStr " "]
htmlUnifier u  = intersperse (Newline []) [ htmlStr $ show v ++ " = " ++ show t | (v,t) <- u ]

modifyLabel node f =
   modify $ relabelNode f node

colorize color label = [Font [Data.GraphViz.Attributes.HTML.Color (X11Color color)] label]

hash :: Path -> Int
-- TODO This is a complicated way to hash a list of integers.
--      Also, a unique hash value would be nice to avoid collisions.
hash = fromEnum . hashString . show

filterOriginal = filter $ \(VariableName n _, _) -> n == 0

hashString = fromIntegral . foldr f 0
  where f c m = ord c + (m * 128) `rem` 1500007
