module ConverterTests(tests) where
import Test.Tasty
import Test.Tasty.HUnit
import qualified HQP.QOp.Syntax as QOp
import HQP.ZX.Convert
import HQP.ZX.Syntax
import Algebra.Graph.Undirected

tests::TestTree
tests =
    testGroup "QC to ZX conversion tests"
        [
            testCase "Z" $
                convert QOp.Z 
                @?= (path 
                    [ Node 0 Input
                    , Node 1 (Green $ PiHalves 2)
                    , Node 2 (Green $ PiHalves 2)]),
            testCase "X" $
                convert QOp.X 
                @?= (path
                    [ Node 0 Input
                    , Node 1 (Red $ PiHalves 2)
                    , Node 2 (Red $ PiHalves 2)]),
            testCase "Rz" $
                convert (QOp.R QOp.Z (pi/2)) 
                @?= (path 
                    [Node 0 Input
                    , Node 1 (Green $ PiHalves 1)
                    , Node 2 (Green $ PiHalves 1)]),
            testCase "Rx" $
                convert (QOp.R QOp.X (pi/2))
                @?= (path 
                [ Node 0 Input
                , Node 1 (Red $ PiHalves 1)
                , Node 2 (Red $ PiHalves 1)]),
            {- Reference: https://zxcalculus.com/figures/cnot.svg
            -G---
              \
            ---R-
            Note that the following two are equivalent representations of CNOT:
            -G-
             |
            -R-
            Or
            ---G-
              /
            -R---
            But we cannot flip the order vertically:
            -R---
              \
            ---G-
            -}
            testCase "CNOT" $
                convert (QOp.C QOp.X)
                @?= ( edges
                    [ (Node 0 Input, Node 2 (Green $ PiHalves 0))
                    , (Node 1 Input, Node 3 (Red $ PiHalves 0))
                    , (Node 2 (Green $ PiHalves 0), Node 3 (Red $ PiHalves 0))
                    , (Node 2 (Green $ PiHalves 0), Node 4 Output)
                    , (Node 3 (Red $ PiHalves 0), Node 5 Output)
                    ])
        ]