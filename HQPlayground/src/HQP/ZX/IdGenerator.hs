
module HQP.ZX.IdGenerator where
import Control.Monad
import HQP.ZX.Syntax
import Data.List
newtype IdGenerator a = IdGenerator
    { runIdGenerator ::
        [Lane] ->
        Depth ->
        Lane ->
        (a,[Lane],Depth,Lane)
    }


instance Functor IdGenerator where
    fmap = liftM

instance Applicative IdGenerator where
    pure a = IdGenerator (\ls d l -> (a,ls,d,l))
    (<*>) = ap

instance Monad IdGenerator where
    m >>= f =
        let transferFunction ls d l =
                let (a,ls', d', l') = runIdGenerator m ls d l
                    result = f a
                in runIdGenerator result ls' d' l'
        in IdGenerator transferFunction

-- Start at lane 0, depth 1, add inputs at depth 0
runGenerator :: IdGenerator a -> Lane -> (a,[Lane],Depth,Lane)
runGenerator m numLanes = runIdGenerator m (indices numLanes) 1 0

generateVertexId :: IdGenerator Id
generateVertexId =
    IdGenerator
    (\lanes depth lane->
        case lanes !? lane of
            Just l -> ((l,depth),lanes, depth, lane)
            Nothing -> error "Tried to generate id for invalid lane."
    )

permute :: [Lane] -> IdGenerator ()
permute permutation =
    IdGenerator
    (\lanes depth lane->
            let beforePerm = take lane lanes
                afterPerm = drop (lane + length permutation) $ lanes
                relPerm =  map (\x -> lanes !! (x+lane)) $ permutation
            in ((),beforePerm ++ relPerm ++ afterPerm, depth, lane + length permutation)
    )

-- Take a step down
switchLane :: IdGenerator ()
switchLane = IdGenerator (\lanes depth lane-> ((),lanes, depth, lane+1))

-- Take a step right and reset lane
proceed :: IdGenerator ()
proceed = IdGenerator (\lanes depth _ -> ((),lanes, depth+1, 0))


indices :: Lane -> [Lane]
indices = reverse . indicesRev

indicesRev :: Lane -> [Lane]
indicesRev 0 = []
indicesRev n = (n-1):(indicesRev $ n-1)