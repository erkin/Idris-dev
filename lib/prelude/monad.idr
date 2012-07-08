module prelude.monad

-- Monads and Functors

import builtins

%access public

infixl 5 >>=

class Monad (m : Set -> Set) where 
    return : a -> m a
    (>>=)  : m a -> (a -> m b) -> m b

class Functor (f : Set -> Set) where 
    map : (a -> b) -> f a -> f b

class Monad m => MonadPlus (m : Set -> Set) where 
    mplus : m a -> m a -> m a
    mzero : m a

guard : MonadPlus m => Bool -> m ()
guard True  = return ()
guard False = mzero

when : Monad m => Bool -> m () -> m ()
when True  f = f
when False _ = return ()
