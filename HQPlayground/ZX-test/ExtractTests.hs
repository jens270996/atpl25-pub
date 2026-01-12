module ExtractTests(tests) where
import Test.Tasty
import Test.Tasty.HUnit
import qualified HQP.QOp.Syntax as QOp
import HQP.ZX.Extract
import HQP.ZX.Syntax
import Algebra.Graph.Undirected
import HQP.QOp.Simplify
tests::TestTree
tests =
    testGroup "Circuit Extraction tests"
        [ semanticsPreservingTests
        ]

extractAndClean :: ZXDiagram -> QOp.QOp
extractAndClean = cleanOnes . cleanOnes . extract
semanticsPreservingTests :: TestTree
semanticsPreservingTests =
    testGroup "Extraction of simple circuits"
    [
        testCase "Z⊗X" $
            extractAndClean ( edges
                    [ (Node (0,0) Input, Node (0,1) (Green $ PiHalves 2))
                    , (Node (1,0) Input, Node (1,1) (Red $ PiHalves 2))
                    , (Node (0,1) (Green $ PiHalves 2), Node (0,2) Output)
                    , (Node (1,1) (Red $ PiHalves 2), Node (1,2) Output)
                    ])
            @?=  (QOp.Tensor QOp.Z QOp.X),
        testCase "Z" $
            extractAndClean ( edges
                    [ (Node (0,0) Input, Node (0,1) (Green $ PiHalves 2))
                    , (Node (0,1) (Green $ PiHalves 2), Node (0,2) Output)
                    ])
            @?=  (QOp.Z),
        testCase "CNOT" $
                extractAndClean ( edges
                    [ (Node (0,0) Input, Node (0,1) (Green $ PiHalves 0))
                    , (Node (1,0) Input, Node (1,1) (Red $ PiHalves 0))
                    , (Node (0,1) (Green $ PiHalves 0), Node (1,1) (Red $ PiHalves 0))
                    , (Node (0,1) (Green $ PiHalves 0), Node (0,2) Output)
                    , (Node (1,1) (Red $ PiHalves 0), Node (1,2) Output)
                    ])
                @?= (QOp.C QOp.X),
        testCase "Compose" $
                extractAndClean ( edges
                    [ (Node (0,0) Input, Node (0,1) (Green $ PiHalves 2))
                    , (Node (0,1) (Green $ PiHalves 2), Node (0,2) (Red $ PiHalves 2))
                    , (Node (0,2) (Red $ PiHalves 2), Node (0,3) Output)
                    ])
                @?= (QOp.Compose QOp.Z QOp.X)
    ]