module SimplifyTests (tests) where
import HQP.ZX.Syntax
import HQP.ZX.Simplify
import Algebra.Graph.Undirected
import Test.Tasty
import Test.Tasty.HUnit

tests::TestTree
tests =
    testGroup "Simplification tests"
        [ mergeSpiderTests
        , removeSimpleSpiderTests
        , removeHadamardsTests
        , alwaysRulesTests
        ]

mergeSpiderTests :: TestTree
mergeSpiderTests =
    testGroup "Merge spider tests"
    [
        testCase "Merging path of reds" $
            mergeRedSpiders (path
                [ Node (0,0) $ Green $ PiHalves 2
                , Node (0,1) $ Red $ PiHalves 2
                , Node (0,2) $ Red $ PiHalves 1
                , Node (0,3) $ Red $ PiHalves 2
                , Node (0,4) $ Green $ PiHalves 2 ])
            @?= path 
                [ Node (0,0) $ Green $ PiHalves 2
                , Node (0,3) $ Red $ PiHalves 1 -- Id 1, 2, 3 could all be correct.
                , Node (0,4) $ Green $ PiHalves 2 ],
        testCase "Merging paths in distinct regions" $
            mergeRedSpiders (path
                [ Node (0,0) $ Green $ PiHalves 2
                , Node (0,1) $ Red $ PiHalves 2
                , Node (0,2) $ Red $ PiHalves 2
                , Node (0,3) $ Green $ PiHalves 2
                , Node (0,4) $ Red $ PiHalves 1
                , Node (0,5) $ Red $ PiHalves 1
                , Node (0,6) $ Green $ PiHalves 2 ])
            @?= path 
                [ Node (0,0) $ Green $ PiHalves 2
                , Node (0,1) $ Red $ PiHalves 0
                , Node (0,3) $ Green $ PiHalves 2
                , Node (0,4) $ Red $ PiHalves 2
                , Node (0,6) $ Green $ PiHalves 2 ],
        testCase "Merging non paths" $
            mergeRedSpiders  (edges -- TODO: Find a more readable way to construct these test cases
                [ (Node (0,0) $ Green $ PiHalves 2, Node (0,1) $ Red $ PiHalves 1)
                , (Node (0,0) $ Green $ PiHalves 2, Node (0,2) $ Red $ PiHalves 2)
                , (Node (0,1) $ Red $ PiHalves 1, Node (0,2) $ Red $ PiHalves 2)
                , (Node (0,1) $ Red $ PiHalves 1, Node (0,3) $ Green $ PiHalves 2) 
                , (Node (0,2) $ Red $ PiHalves 2, Node (0,3) $ Green $ PiHalves 2) ])
            @?= edges 
                [ (Node (0,0) $ Green $ PiHalves 2, Node (0,1) $ Red $ PiHalves 3)
                , (Node (0,1) $ Red $ PiHalves 3, Node (0,3) $ Green $ PiHalves 2) ]
    ]

removeSimpleSpiderTests :: TestTree
removeSimpleSpiderTests =
    testGroup "Remove simple spider tests"
    [
        testCase "Removing single zero phase simple spider" $
            removeSimpleSpiders  (path
                [ Node (0,0) $ Green $ PiHalves 2
                , Node (0,1) $ Red $ PiHalves 0
                , Node (0,2) $ Green $ PiHalves 2 ])
            @?= path 
                [ Node (0,0) $ Green $ PiHalves 2
                , Node (0,2) $ Green $ PiHalves 2 ],
        testCase "Removing multiple zero phase simple spiders" $
            removeSimpleSpiders  (path
                [ Node (0,0) Input
                , Node (0,1) $ Green $ PiHalves 2
                , Node (0,2) $ Red $ PiHalves 0
                , Node (0,3) $ Green $ PiHalves 0
                , Node (0,4) Output])
            @?= path 
                [ Node (0,0) Input
                , Node (0,1) $ Green $ PiHalves 2
                , Node (0,4) Output]
    ]

removeHadamardsTests :: TestTree
removeHadamardsTests =
    testGroup "Remove Hadamard tests"
    [
        testCase "Removing two Hadamards on same edge" $
            removeHadamards  (path
                [ Node (0,0) Input
                , Node (0,1) H
                , Node (0,2) H
                , Node (0,3) Output ])
            @?= path 
                [ Node (0,0) $ Input
                , Node (0,3) $ Output ]
    ]

alwaysRulesTests :: TestTree
alwaysRulesTests =
    testGroup "Always applicable rules tests"
    [
        testCase "Always rule 2 test" $
            alwaysRule2 (path
                [ Node (0,0) Input
                , Node (0,1) $ Red $ PiHalves 2
                , Node (0,2) $ Green $ PiHalves 2
                , Node (0,3) $ Red $ PiHalves 2
                , Node (0,4) Output ])
            @?= path
                [ Node (0,0) Input
                , Node (0,2) $ Green $ PiHalves 2
                , Node (0,4) Output ],
        testCase "Always rule 2 negative test" $
            alwaysRule2 (path
                [ Node (0,0) Input
                , Node (0,1) $ Red $ PiHalves 0
                , Node (0,2) $ Green $ PiHalves 2
                , Node (0,3) $ Red $ PiHalves 2
                , Node (0,4) Output ])
            @?= path
                [ Node (0,0) Input
                , Node (0,1) $ Red $ PiHalves 0
                , Node (0,2) $ Green $ PiHalves 2
                , Node (0,3) $ Red $ PiHalves 2
                , Node (0,4) Output ],
        testCase "Always rule 5 test" $
            alwaysRule5 (path
                [ Node (0,0) Input
                , Node (0,1) $ Green $ PiHalves 2
                , Node (0,2) $ Red $ PiHalves 2
                , Node (0,3) $ Green $ PiHalves 2
                , Node (0,4) Output ])
            @?= path
                [ Node (0,0) Input
                , Node (0,2) $ Red $ PiHalves 2
                , Node (0,4) Output ],
        testCase "Always rule 6 test" $
            alwaysRule6 (path
                [ Node (0,0) Input
                , Node (0,1) $ Red $ PiHalves 1
                , Node (0,2) $ Green $ PiHalves 1
                , Node (0,3) $ Red $ PiHalves 1
                , Node (0,4) $ Green $ PiHalves 1
                , Node (0,5) Output ])
            @?= path
                [ Node (0,0) Input
                , Node (0,1) $ Red $ PiHalves 2
                , Node (0,2) $ Green $ PiHalves 1
                , Node (0,3) $ Red $ PiHalves 1
                , Node (0,5) Output ]
    ]