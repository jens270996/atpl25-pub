module HQP.ZX.Extract where
import HQP.ZX.Syntax
import HQP.ZX.Utils
import Algebra.Graph.Undirected
import Data.List
import Data.Maybe
import Debug.Trace
import qualified HQP.QOp.Syntax as QOp

extract :: ZXDiagram -> QOp.QOp
extract g =
    let inputs = filter (\(Node _ el) -> el == Input) $ vertexList g
        circ = QOp.One
        (resG,circ',resBound) = sweepBoundary (foldr removeVertex g inputs,circ,map (nextInLane g) inputs)

    in case resG of
        empty -> circ'
        g -> error ("Residual graph" ++ show g ++ "\n")

sweepBoundary :: (ZXDiagram,QOp.QOp,[ZXNode]) -> (ZXDiagram,QOp.QOp,[ZXNode])
-- sweepBoundary (g,circ,boundary) | trace ("sweepBoundary " ++ show g ++ " " ++ show circ ++ " " ++ show boundary) False = undefined
sweepBoundary (g,circ,boundary) | isEmpty g  = (g,circ,boundary) -- Check that all nodes in boundary are outputs
sweepBoundary (g,circ,boundary) | all (isOutput) boundary  = (g,circ,boundary) -- Check that all nodes in boundary are outputs
sweepBoundary (g,circ,boundary) =
    let (g',boundary') = foldr (splitSpiders boundary) (g,[]) boundary
        -- Check if permutation is required
        (circ',boundary'') = permuteBoundary boundary' g' circ
        -- Turn boundary into quantumCirquit
        newCircuit = circ' <> (extractCircuit boundary'')
        -- Find new boundary
        newBoundary = map (nextInLane g) boundary''
        -- Remove old boundary from graph
    in sweepBoundary (foldr removeVertex g boundary'',newCircuit,newBoundary)

--split a spider and return the root vertex of the set of split vertices
splitSpiders :: [ZXNode] -> ZXNode -> (ZXDiagram,[ZXNode]) -> (ZXDiagram,[ZXNode])
-- splitSpiders boundary n (g,acc) | trace ("splitSpiders " ++ show boundary ++ " " ++ show g ++ " " ++ show n ++ " " ++ show acc) False = undefined
splitSpiders boundary n (g,acc) = case isQuantumGate g n || isInput n || isOutput n of
                    True | asPhase n == (Just $ PiHalves 0) -- Align CNOTs
                         , all (`notElem` boundary) (getNeighbors n g)
                         -> let wire = decrementDepth (Node (getVertexId n) Wire)
                            in  (overlay g (edge wire n),wire:acc)
                    True -> (g,n:acc)
                    False | asPhase n /= (Just $ PiHalves 0) ->
                        -- shot out zero spider with remaining legs.
                        -- keep root spider with phase
                        let zeroSpider = (Node (getVertexId n) (if isGreen n then Green (PiHalves 0) else Red (PiHalves 0)))
                            g = replaceVertex n zeroSpider g
                            -- update id for n
                            n =  decrementDepth n
                        in (overlay g (edge n zeroSpider),n:acc)
                    _ -> -- 0 spider with more than 3 legs
                        let neighs = getNeighbors n g
                            neigh = head $ filter (\n' -> vertexLane n' /= vertexLane n) neighs
                            -- give root new id
                            root =  decrementDepth n
                            -- remove edge from zeroSpider to neigh
                            g = removeEdge n neigh g
                            -- connect root to neigh
                            g' = overlay g $ edge root neigh
                            -- connect root to zeroSpider
                            g'' = overlay g' $ edge root n
                        in (g'',root:acc)


permuteBoundary :: [ZXNode] -> ZXDiagram -> QOp.QOp -> (QOp.QOp,[ZXNode])
-- permuteBoundary boundary g circ | trace ("permuteBoundary " ++ show g ++ " " ++ show circ ++ " " ++ show boundary) False = undefined
permuteBoundary boundary g circ | permutationRequired boundary g =
    let controls = filter (\(Node _ c) ->  c == (Green (PiHalves 0))) boundary
        xs = map (\c -> head . filter (\x -> hasEdge c x g && getElement x == (Red (PiHalves 0))) $ boundary) controls
        -- reorder boundary according to permutation
        cNots = merge controls xs
        newBoundary = cNots ++ filter (\x -> x `notElem` cNots) boundary
        -- compose permutation with current circ
        perm = catMaybes . map (\n -> elemIndex n newBoundary) $ boundary
    in (circ <> (QOp.Permute perm),newBoundary)
permuteBoundary boundary _ circ = (circ,boundary)

extractCircuit :: [ZXNode] -> QOp.QOp
-- extractCircuit boundary | trace ("extractCircuit " ++ show boundary) False = undefined
extractCircuit ((Node _ (Green (PiHalves 0))):((Node _ (Red (PiHalves 0))):boundary)) = QOp.Tensor  (QOp.C QOp.X) (extractCircuit boundary)
extractCircuit  ((Node _ (Green (PiHalves 2))):boundary) =  QOp.Tensor QOp.Z (extractCircuit boundary)
extractCircuit  ((Node _ (Red (PiHalves 2))):boundary) =  QOp.Tensor QOp.X (extractCircuit boundary)
extractCircuit  ((Node _ (Green (Frac a))):boundary) =  QOp.Tensor (QOp.R QOp.Z a) (extractCircuit boundary)
extractCircuit  ((Node _ (Red (Frac a))):boundary) = QOp.Tensor (QOp.R QOp.X a) (extractCircuit boundary)
extractCircuit  ((Node _ H):boundary) = QOp.Tensor QOp.H (extractCircuit boundary)
extractCircuit  ((Node _ Output):boundary) = QOp.Tensor (QOp.Id 1) (extractCircuit boundary)
extractCircuit [] = QOp.One
extractCircuit _ = error "Missing case in extractCircuit"

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs
                             
permutationRequired :: [ZXNode] -> ZXDiagram -> Bool
-- permutationRequired boundary g | trace ("permutationRequired " ++ show g ++ " " ++ show boundary) False = undefined
permutationRequired ((Node _ (Red (PiHalves 0))):_) _ = True
permutationRequired ((Node id' (Green (PiHalves 0))):((Node id'' (Red (PiHalves 0))):boundary)) g = hasEdge (Node id' (Green (PiHalves 0))) (Node id'' (Red (PiHalves 0))) g || permutationRequired boundary g
permutationRequired [] _ = False
permutationRequired (_:boundary) g = permutationRequired boundary g

isQuantumGate :: ZXDiagram -> ZXNode -> Bool
isQuantumGate g n = case getNeighbors n g of
                    [n'] | vertexLane n' == vertexLane n -> True
                    [n',n''] | vertexLane n' == vertexLane n
                               || vertexLane n'' == vertexLane n
                               && asPhase n == (Just $ PiHalves 0) -> True
                    _ -> False