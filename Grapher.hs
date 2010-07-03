--
-- Grapher.hs
--
-- Convert a cost center profile generated by the -px flag
-- into the directed graph in the Dot format of GraphViz
--
-- Gregory Wright, 28 July 2007
--
-- Copyright (c) 2007, 2008 Antiope Associates LLC, all rights reserved.
--

module Grapher (
        ColorCoord (..),
        GraphOptions (..),
        NodeStyle (..),
        profToDot
) where


import Control.Exception
import Data.IntMap
import Data.List as List
import Data.Maybe

import GraphUtils
import ParseProfile


-- | How to color a graph.
--
data ColorCoord = NoColor | ColorCalls | ColorTicks | ColorAllocs
                deriving (Eq, Show)


data NodeStyle = Brief | Verbose
               deriving (Eq, Show)


-- | The GraphOptions record tells us how to color the graph, whether
--   to do call tree -> call graph conversion, whether nodes should be displayed
--   in a brief format (just the node name) and if modules should be shown by
--   a labelled box.
--
data GraphOptions = GraphOptions { graphDebug         :: Bool,
                                   graphColorCoord    :: ColorCoord,
                                   graphNodeStyle     :: NodeStyle,
                                   graphAnnotateEdges :: Bool,
                                   graphShowModules   :: Bool }
        deriving Show


-- | Our data types are nodes and edges. A Node is a collection of all
--   of the profiling information for a cost center. The Node record is
--   defined in ParseProfile.hs.  An edge is simply a pair of Ints identifying
--   the connected Nodes.
--
type Edge = (Int, Int, Int)


-- | Looks up the cost (call count, ticks, allocs) of a given edge.
--
lookupEdgeCost :: Node -> Int -> Int -> (Integer, Integer, Integer)
lookupEdgeCost node from st = let
        ci  = parentNodes node
        ci' = List.filter (\n -> parentNodeNumber n == from && stackNumber n == st) ci
        ch  = head ci'
        in
          assert (length ci' == 1) (counts ch, ticks ch, allocs ch)


-- | extractEdges extracts the edges of the call graph from a Profile.
--   The return value is a lists of all the edges in the graph connecting
--   cost centers.
--
extractEdges :: Profile -> [ Edge ]
extractEdges p = let
        edgePairs :: (Int, Node) -> [ Edge ]
        edgePairs (nn, node) = if List.null (parentNodes node)
                                   then []
                                   else List.map (\ci -> (stackNumber ci, parentNodeNumber ci, nn)) (parentNodes node)
        in
          concatMap edgePairs (toList (profileGraph p))
               

-- | extractNodes takes the profile and returns a list of pairs with the
--   (node number, node info).  It is now just a synonym for toList, but
--   is given a definition as a placeholder in case more involved procesing
--   is required.
--
extractNodes :: Profile -> [ (Int, Node) ]
extractNodes p = toList (profileGraph p)


-- | The maxCost function computes the maximum resource usage of all the
--   nodes.  It is used when attributing fractional costs to particular
--   nodes in the graph.  It returns a CallInfo record containing the maximum
--   calls, ticks and allocations.  Note that the maxima of the different
--   fields may be due to different nodes.
--
maxCost :: [ (Int, Node) ] -> CallInfo
maxCost ns = foldl' maxCosts CallInfo {parentNodeNumber = undefined, stackNumber = undefined, counts = 0, ticks = 0, allocs = 0} ns
               where
                 maxCosts :: CallInfo -> (Int,  Node) -> CallInfo
                 maxCosts c (_, n) = let
                         (cs, ts, as) = totalCost (parentNodes n)
                         in
                           CallInfo { parentNodeNumber = undefined,
                                      stackNumber      = undefined,
                                      counts = maxOf (counts c) cs,
                                      ticks  = maxOf (ticks c)  ts,
                                      allocs = maxOf (allocs c) as }
                 maxOf a b = if a >= b then a else b


-- | extractModules return a list of pairs containing the module name and
--   all of the nodes associated with it.
--
extractModules :: Profile -> [ (String, [ Int ]) ]
extractModules p = let
        modulesAndNodes = List.map (\(n, node) -> (nodeModule node, n)) (extractNodes p)

        groupModules :: (String, Int) -> (String, Int) -> Bool
        groupModules (s1, _) (s2, _) = s1 == s2

        sortModules :: (String, Int) -> (String, Int) -> Ordering
        sortModules (s1, _) (s2, _) = if s1 == s2
                                            then EQ
                                            else if s1 > s2
                                                 then GT
                                                 else LT

        sortedModules = sortBy sortModules modulesAndNodes
        moduleList    = List.map unzip (groupBy groupModules sortedModules)
        in
          List.map (\(mods, ns) -> (head mods, ns)) moduleList


-- | Apply a color map.  The colorize function takes the total resource
--   usage (count, ticks or allocs) and a selector function for the same
--   field in the nodeCost record, returning a function which determines
--   the color of the node based on its resource usage.
--
colorize :: Integer -> (CallInfo -> Integer) -> Node -> String
colorize costBasis selector node = let
        costBasis' = if costBasis /= 0 then (fromInteger costBasis) else 1.0
        (cs, ts, as) = totalCost (parentNodes node)
        cost = CallInfo { parentNodeNumber = undefined, stackNumber = undefined, counts = cs, ticks = ts, allocs = as}
        fractionalCost = fromInteger (selector cost) /  costBasis'
        maxhue = 0.6
        minsat = 0.1

        hue = maxhue * (1.0 - fractionalCost)
        sat = minsat + (3.0 - minsat) * fractionalCost
        val = 1.0 :: Double

        sat' = if sat <= 1.0 then sat else 1.0 :: Double
        in
          "color = \"" ++ show hue ++ ", " ++ show sat' ++ ", " ++ show val ++ "\" "


-- | The pieces of a dot file: the foreword, the edge descriptions, the
--   node descriptions, possibly subgraphs grouping the modules and the afterword:
--
foreword :: String -> String
foreword name = "digraph \"" ++ name ++ "\" {\n"


nodeColorStyle :: ColorCoord -> String
nodeColorStyle NoColor = ""
nodeColorStyle _       = "node [style = filled];\n"


afterword :: String
afterword = "}\n"


showEdge :: GraphOptions -> Profile -> Edge -> String
showEdge opts p (st, from, to) = let
        costs              = lookupEdgeCost ((profileGraph p) ! to) from st
        label (cs, ts, as) = if graphDebug opts
                             then " [ label = \"#" ++ show st ++
                                           "\\l (" ++ show cs ++ ", "
                                                   ++ show ts ++ ", "
                                                   ++ show as ++ ")\" ];"
                             else if graphAnnotateEdges opts
                                  then " [ label = \"(" ++ show cs ++ ", "
                                                        ++ show ts ++ ", "
                                                        ++ show as ++ ")\" ];"
                                  else ""
        showEdge' (cs, ts, as) = (show from ++ " -> " ++ show to) ++ (label (cs, ts, as)) ++ "\n"
        in
          showEdge' costs

 
showNode :: GraphOptions -> Maybe (Node -> String) -> (Int, Node) -> String
showNode opts cm (nn, node) = let
        color = if isJust cm
                    then (fromJust cm) node
                    else ""
        in
          show nn ++ " [ shape=box, " ++
                         color        ++
                         "label=\""   ++ showNodeInfo opts node ++ "\" ];\n"


showNodeInfo :: GraphOptions -> Node -> String
showNodeInfo opts node = let
        modul  = if graphDebug opts || not (graphShowModules opts) then nodeModule node ++ "\\n" else ""
        name   = nodeName node
        number = if graphDebug opts then "\\n#" ++ show (nodeNumber node) else ""

        (cs, ts, as) = totalCost (parentNodes node)

        nodeInfo = if graphDebug opts || graphNodeStyle opts == Verbose
                       then "\\ncount = "  ++ show cs ++
                            "\\nticks = "  ++ show ts ++
                            "\\nallocs = " ++ show as ++ "\\n"
                       else ""

        in
          modul ++ name ++ number ++ nodeInfo


showModule :: (String, [ Int ]) -> String
showModule (m, ccs) = "subgraph \"cluster_" ++ m ++
                      "\" { label = \""     ++ m ++
                            "\"; "               ++
                            "style=filled; color=lightgrey; "    ++
                            concatMap (\n -> show n ++ "; ") ccs ++
                        " }\n"


profToDot :: GraphOptions -> String -> Profile -> String
profToDot options name p = let
         p' = if graphDebug options
                  then p
                  else p { profileGraph = (collapseParallel . prune) (profileGraph p) }

         es = extractEdges p'
         ns = extractNodes p'

         colorMap = let
                        m = maxCost ns
                    in
                        case graphColorCoord options of
                          NoColor     -> Nothing
                          ColorCalls  -> Just (colorize (counts m) counts)
                          ColorTicks  -> Just (colorize (ticks m)  ticks)
                          ColorAllocs -> Just (colorize (allocs m) allocs)

         edges = concatMap (showEdge options p') es
         nodes = concatMap (showNode options colorMap) ns
         mods  = if graphShowModules options
                     then concatMap showModule (extractModules p')
                     else ""

         style = nodeColorStyle (graphColorCoord options)

         in
           foreword name ++
           style         ++
           edges         ++
           nodes         ++
           mods          ++
           afterword
