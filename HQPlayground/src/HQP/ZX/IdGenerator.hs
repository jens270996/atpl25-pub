
module HQP.ZX.IdGenerator where
import Control.Monad
import HQP.ZX.Syntax
newtype IdGenerator a = IdGenerator
    { runIdGenerator ::
        Id ->
        (a,Id)
    }


instance Functor IdGenerator where
    fmap = liftM

instance Applicative IdGenerator where
    pure a = IdGenerator (\c -> (a,c))
    (<*>) = ap

instance Monad IdGenerator where
    m >>= f =
        let transferFunction c =
                let (a,c') = runIdGenerator m c
                    result = f a
                in runIdGenerator result c'
        in IdGenerator transferFunction

generateVertexId :: IdGenerator Id
generateVertexId = IdGenerator (\c-> (c,c))

-- Take a step down
switchLane :: IdGenerator ()
switchLane = IdGenerator (\(lane,depth)-> ((),(lane+1,depth)))

-- Take a step right
proceed :: IdGenerator ()
proceed = IdGenerator (\(lane,depth)-> ((),(lane,depth+1)))

getLane :: IdGenerator Lane
getLane = IdGenerator (\(lane,depth)-> (lane,(lane,depth)))

getDepth :: IdGenerator Depth
getDepth = IdGenerator (\(lane,depth)-> (depth,(lane,depth)))

setLane :: Lane -> IdGenerator ()
setLane lane = IdGenerator (\(_,depth)-> ((),(lane,depth)))

setDepth :: Depth -> IdGenerator ()
setDepth depth = IdGenerator (\(lane,_)-> ((),(lane,depth)))