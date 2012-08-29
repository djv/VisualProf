--
-- GraphUtils.hs
--
-- Copyright (c) 2007, 2008 Antiope Associates LLC, all rights reserved.
--

module GraphUtils (
        collapseParallel,
        prune,
        totalCost
) where

import Data.IntMap as IntMap
import Data.List as List

import ParseProfile


-- For a list of calls, compute the sum of the counts (number of
-- calls), ticks and allocs
--
totalCost :: [ CallInfo ] -> (Integer, Integer, Integer)
totalCost cis = let
         cis' = nubBy (\x y -> stackNumber x == stackNumber y) cis
         in
           List.foldl (\(c, t, a) ci -> (c + counts ci, t + ticks ci, a + allocs ci)) (0, 0, 0) cis'


collapseCost :: [ CallInfo ] -> CallInfo
collapseCost = List.foldl (\ci ci' -> ci { parentNodeNumber = parentNodeNumber ci',
                                      stackNumber      = stackNumber ci',
                                      counts           = counts ci + counts ci',
                                      ticks            = ticks  ci + ticks ci',
                                      allocs           = allocs ci + allocs ci'} ) emptyCallInfo
               where
                 emptyCallInfo = CallInfo { parentNodeNumber = undefined,
                                            stackNumber      = undefined,
                                            counts           = 0,
                                            ticks            = 0,
                                            allocs           = 0 }

prunable :: Node -> Bool
prunable n = isLeaf n && totalCost (parentNodes n) == (0, 0, 0)


-- The first thing to do is to find all the leaf nodes and delete
-- the ones that have no costs associated with them. The pruneOnce
-- function deletes the zero cost leaf nodes and returns a pair
-- of the number of changes made and the pruned profile graph.
--
pruneOnce :: ProfileGraph -> (Int, ProfileGraph)
pruneOnce g = let
        pruneNode :: (Int, ProfileGraph) -> Int -> (Int, ProfileGraph)
        pruneNode (i, g') nc = if prunable (g' ! nc)
                                  then (i + 1, IntMap.delete nc g')
                                  else (i, g')
        (nChanges, g'') = List.foldl pruneNode (0, g) (keys g)
        in
          (nChanges, markParents g'')

-- repeatedly prune until there are no more changes
--
prune :: ProfileGraph -> ProfileGraph
prune g = let
        (nChanges, g') = pruneOnce g
        in
          if nChanges == 0 then g' else prune g'


-- Collapse parallel edges to a single edge
--
collapseParallel :: ProfileGraph -> ProfileGraph
collapseParallel = IntMap.map collapseParents


-- For a given node n, collapse all of the parents with the same
-- parentNode number.
--
collapseParents :: Node -> Node
collapseParents n = let
        ps  = parentNodes n
        ps' = group (sort ps)
        ci  = List.map collapseCost ps'
        in
          n { parentNodes = ci }

