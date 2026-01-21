module HQP.ZX.Visualize where
import Diagrams.Prelude
import HQP.ZX.Syntax
import Algebra.Graph.Undirected
import Diagrams.Backend.SVG.CmdLine
import HQP.ZX.Utils
import Data.List


visualizeDiagram :: ZXDiagram -> Diagram B
visualizeDiagram g =
    let
        es = edgeList g
    in visualizeVertices g # visualizeEdges es

visualizeVertices ::  ZXDiagram -> Diagram B
visualizeVertices =
    vsep 2 . map visualizeLane . lanes


visualizeLane :: [ZXNode] -> Diagram B
visualizeLane ns = hsep 5 . map visualizeNode $ ns

visualizeNode :: ZXNode -> Diagram B
visualizeNode (Node id e) = visualizeElement e # named (show id)

visualizeElement :: ZXElement -> Diagram B
visualizeElement H = square 0.4 # fc yellow
visualizeElement (Red (PiHalves 0)) = circle 0.5 # fc red
visualizeElement (Green (PiHalves 0)) = circle 0.5 # fc green
visualizeElement (Red p) = (text $ show p) <> circle 1 # fc red
visualizeElement (Green p) =  (text $ show p ) <> circle 1 # fc green
visualizeElement Input = text "in" <> circle 1 
visualizeElement Output = text "out" <> circle 1 


lanes :: ZXDiagram -> [[ZXNode]]
lanes = groupBy sameLane . vertexList

visualizeInputs :: ZXDiagram -> Diagram B
visualizeInputs g = vsep 1 (map visualizeElement $ getInputs g)
visualizeOutputs :: ZXDiagram -> Diagram B
visualizeOutputs g = vsep 1 (map visualizeElement $ getOutputs g)

visualizeElements :: ZXDiagram -> Diagram B
visualizeElements g = vsep 0.2 (map visualizeElement $ getElementsWhere g (\x -> x/=Output && x/=Input))

getInputs :: ZXDiagram -> [ZXElement]
getInputs g = getElementsWhere g (Input==)

getOutputs :: ZXDiagram -> [ZXElement]
getOutputs g = getElementsWhere g (Output==)

getElementsWhere :: ZXDiagram -> (ZXElement -> Bool) -> [ZXElement]
getElementsWhere g pred = filter pred . map getElement $ vertexList g 

-- Use arrow to connect
visualizeEdges :: [(ZXNode,ZXNode)] -> Diagram B -> Diagram B
visualizeEdges es d = foldr visualizeEdge d es


visualizeEdge :: (ZXNode,ZXNode) -> (Diagram B -> Diagram B)
visualizeEdge ((Node id1 _),(Node id2 _)) = (\d -> d # connectOutside' (with & arrowTail .~ noTail & lengths .~ large
                 & arrowHead .~ noHead) (show id1) (show id2))