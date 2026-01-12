module HQP.ZX.Extract where
import HQP.ZX.Syntax
import HQP.ZX.Utils
import Algebra.Graph.Undirected
import Data.List
import Data.Maybe
import qualified HQP.QOp.Syntax as QOp

extract :: ZXDiagram -> QOp.QOp
extract g =
    let inputs = filter (\(Node _ el) -> el == Input) $ vertexList g
        circuit = QOp.Id $ length inputs
    in case sweepBoundary (g,circuit,inputs) of
        (empty,circuit,[]) -> circuit
    -- peel off inputs
    -- progress each lane one step to get a tensor QOp.
    -- recursively compose until reaching outputs.
    -- at each layer fix all the stuff that needs to be done.
    -- TODO: What if a CNOT gate has been flipped?
sweepBoundary :: (ZXDiagram,QOp.QOp,[ZXNode]) -> (ZXDiagram,QOp.QOp,[ZXNode])
sweepBoundary (g,circuit,boundary) | isEmpty g  = (g,circuit,boundary) -- Check that all nodes in boundary are outputs
sweepBoundary (g,circuit,boundary) =
    -- Split multilegged spiders
    let (g,boundary) = foldr splitSpiders (g,[]) boundary
        -- Check if permutation is required
        (circuit,boundary') = permuteBoundary boundary g circuit   
        -- Turn boundary into quantumCirquit
        newCircuit = circuit <> (extractCircuit boundary')
        -- Find new boundary
        newBoundary = map (nextInLane g) boundary'
        -- Remove old boundary from graph
    in sweepBoundary (foldr removeVertex g boundary',newCircuit,newBoundary)

--split a spider and return the root vertex of the set of split vertices
splitSpiders :: ZXNode -> (ZXDiagram,[ZXNode]) -> (ZXDiagram,[ZXNode])
splitSpiders n (g,boundary) = case isQuantumGate g n of
                    True -> (g,n:boundary)
                    False | asPhase n /= (Just $ PiHalves 0) ->
                        -- shot out zero spider with remaining legs.
                        -- keep root spider with phase
                        let zeroSpider = (Node (getVertexId n) (if isGreen n then Green 0 else Red 0))
                            g = replaceVertex n zeroSpider g
                            -- update id for n
                            n =  decrementDepth n
                        in (overlay g (edge n zeroSpider),n:boundary)
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
                        in (g'',root:boundary)

permuteBoundary :: [ZXNode] -> ZXDiagram -> QOp.QOp -> (QOp.QOp,[ZXNode])
permuteBoundary boundary g circuit | permutationRequired boundary g =
    let controls = filter (\(Node id' c) ->  c == (Green (PiHalves 0))) boundary
        xs = map (\c -> head . filter (\x -> hasEdge c x g && getElement x == (Red (PiHalves 0))) $ boundary) controls
        -- reorder boundary according to permutation
        depPairs = merge controls xs
        newBoundary = depPairs ++ filter (\x -> x `notElem` depPairs) boundary
        -- compose permutation with current circuit
        perm = catMaybes . map (\n -> elemIndex n newBoundary) $ boundary
    in (circuit <> (QOp.Permute perm),newBoundary)
permuteBoundary boundary _ circuit = (circuit,boundary)

extractCircuit :: [ZXNode] -> QOp.QOp
extractCircuit ((Node _ (Green (PiHalves 0))):((Node _ (Red (PiHalves 0))):boundary)) = QOp.Tensor  (QOp.C QOp.X) (extractCircuit boundary)
extractCircuit  ((Node _ (Green (PiHalves 2))):boundary) =  QOp.Tensor QOp.Z (extractCircuit boundary)
extractCircuit  ((Node _ (Red (PiHalves 2))):boundary) =  QOp.Tensor QOp.X (extractCircuit boundary)
extractCircuit  ((Node _ (Green (Frac a))):boundary) =  QOp.Tensor (QOp.R QOp.Z a) (extractCircuit boundary)
extractCircuit  ((Node _ (Red (Frac a))):boundary) = QOp.Tensor (QOp.R QOp.X a) (extractCircuit boundary)
extractCircuit  ((Node _ H):boundary) = QOp.Tensor QOp.H (extractCircuit boundary)

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs
                             
permutationRequired :: [ZXNode] -> ZXDiagram -> Bool
permutationRequired ((Node _ (Red (PiHalves 0))):boundary) _ = True
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