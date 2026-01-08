module HQP.ZX.Simplify where
import HQP.ZX.Syntax
import HQP.ZX.Utils
import Algebra.Graph.Undirected
import Data.Maybe (mapMaybe)
import Data.Function ((&))

--https://hackage.haskell.org/package/algebraic-graphs-0.7/docs/Algebra-Graph.html

-- If a spider has phase 0 and two legs, it can be removed.
removeSimpleSpiders :: ZXDiagram -> ZXDiagram
removeSimpleSpiders graph = foldv helper graph
  where
    helper node graph'
        | Just (PiHalves 0) <- asPhase node
        , neighbor : ns <- getNeighbors node graph'
        , length ns <= 1
        = replaceVertex node neighbor $ removeEdge node neighbor graph'
        | otherwise = graph'

-- Hadamard are self-inverse, so two Hadamard gates on the same edge cancel out.
removeHadamards :: ZXDiagram -> ZXDiagram
removeHadamards graph = foldv helper graph
  where
    helper h1 graph'
        | isHadamard h1
        , [h2, otherNeighbor] <- getNeighbors h1 graph'
        , isHadamard h2
        = replaceVertex h1 otherNeighbor graph' & replaceVertex h2 otherNeighbor & removeEdge otherNeighbor otherNeighbor
        | otherwise = graph'

-- If same-color spiders are connected, they can be merged into a single spider with phase equal to the sum of the phases.
mergeRedSpiders :: ZXDiagram -> ZXDiagram
mergeRedSpiders graph = foldv helper graph
  where
    helper node graph'
        | Just (nid, nPhase) <- asRed node
        = let (nids, phases) = unzip $ (nid, nPhase) : mapMaybe asRed (getNeighbors node graph')
              newSpider = Node nid (Red $ sum phases)
              newGraph = mergeVertices ((`elem` nids) . getVertexId) newSpider graph'
          in removeEdge newSpider newSpider newGraph
        | otherwise = graph'

mergeGreenSpiders :: ZXDiagram -> ZXDiagram
mergeGreenSpiders graph = foldv helper graph
  where
    helper node graph'
        | Just (nid, nPhase) <- asGreen node
        = let (nids, phases) = unzip $ (nid, nPhase) : mapMaybe asGreen (getNeighbors node graph')
              newSpider = Node nid (Green $ sum phases)
              newGraph = mergeVertices ((`elem` nids) . getVertexId) newSpider graph'
          in removeEdge newSpider newSpider newGraph
        | otherwise = graph'

-- If a spider has a majority of Hadamard gates on its legs, change its color??
-- changeColorOnMajority :: ZXDiagram -> ZXDiagram

alwaysRule2 :: ZXDiagram -> ZXDiagram
alwaysRule2 graph = foldv helper graph
  where
    helper node graph'
        | Just (_, PiHalves 2) <- asGreen node
        , [n1, n2] <- getNeighbors node graph'
        , Just (_, PiHalves 2) <- asRed n1
        , Just (_, PiHalves 2) <- asRed n2
        = replaceVertex n1 node graph' & replaceVertex n2 node & removeEdge node node
        | otherwise = graph'

-- This is alwaysRule2 with colors swapped
alwaysRule5 :: ZXDiagram -> ZXDiagram
alwaysRule5 graph = foldv helper graph
  where
    helper node graph'
        | Just (_, PiHalves 2) <- asRed node
        , [n1, n2] <- getNeighbors node graph'
        , Just (_, PiHalves 2) <- asGreen n1
        , Just (_, PiHalves 2) <- asGreen n2
        = replaceVertex n1 node graph'
          & replaceVertex n2 node
          & removeEdge node node
        | otherwise = graph'

-- Red1(+)-Green1(+)-Red2(+)-Green2(+) -> Red1(pi)-Green1(+)-Red2(+)
alwaysRule6 :: ZXDiagram -> ZXDiagram
alwaysRule6 graph = foldv helper graph
  where 
    helper green1 graph'
        | Just (_, PiHalves 1) <- asGreen green1
        , [red1, red2] <- getNeighbors green1 graph'
        , Just (nid, PiHalves 1) <- asRed red1
        , Just (_, PiHalves 1) <- asRed red2
        , [n1, n2] <- getNeighbors red2 graph'
        , [green2] <- filter (/= green1) [n1, n2]
        = replaceVertex red1 (Node nid $ Red $ PiHalves 2) graph'
          & replaceVertex green2 red2
          & removeEdge red2 red2
        | otherwise = graph'