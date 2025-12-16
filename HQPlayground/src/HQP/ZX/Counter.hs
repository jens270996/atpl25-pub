
module HQP.ZX.Counter where
import Control.Monad

newtype Counter a = Counter
    { runCounter ::
        Int ->
        (a,Int)
    }


instance Functor Counter where
    fmap = liftM

instance Applicative Counter where
    pure a = Counter (\c -> (a,c))
    (<*>) = ap

instance Monad Counter where
    m >>= f =
        let transferFunction c =
                let (a,c') = runCounter m c
                    result = f a
                in runCounter result c'
        in Counter transferFunction

generateVertexId :: Counter Int
generateVertexId = Counter (\c-> (c,c+1))