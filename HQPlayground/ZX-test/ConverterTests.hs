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
                    [ Node (0,0) Input
                    , Node (0,1) (Green $ PiHalves 2)
                    , Node (0,2) Output]),
            testCase "X" $
                convert QOp.X 
                @?= (path
                    [ Node (0,0) Input
                    , Node (0,1) (Red $ PiHalves 2)
                    , Node (0,2) Output]),
            testCase "Rz" $
                convert (QOp.R QOp.Z (fromIntegral 7/8)) 
                @?= (path 
                    [Node (0,0) Input
                    , Node (0,1) (Green $ Frac (fromIntegral 7/8))
                    , Node (0,2) Output]),
            testCase "Rx" $
                convert (QOp.R QOp.X (fromIntegral 7/8))
                @?= (path 
                [ Node (0,0) Input
                , Node (0,1) (Red $ Frac (fromIntegral 7/8))
                , Node (0,2) Output]),
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
                    [ (Node (0,0) Input, Node (0,1) (Green $ PiHalves 0))
                    , (Node (1,0) Input, Node (1,1) (Red $ PiHalves 0))
                    , (Node (0,1) (Green $ PiHalves 0), Node (1,1) (Red $ PiHalves 0))
                    , (Node (0,1) (Green $ PiHalves 0), Node (0,2) Output)
                    , (Node (1,1) (Red $ PiHalves 0), Node (1,2) Output)
                    ])
        ]