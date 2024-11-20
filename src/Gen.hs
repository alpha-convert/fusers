{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Gen where
import Language.Haskell.TH
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Maybe

{-
All of this is stolen from 
https://github.com/AndrasKovacs/staged/blob/main/icfp24paper/supplement/haskell-cftt/CFTT/Gen.hs
-}

newtype Gen a = Gen {unGen :: forall r. (a -> CodeQ r) -> CodeQ r}


runGen :: Gen (CodeQ a) -> CodeQ a
runGen (Gen f) = f id

instance Functor Gen where
  fmap f ma = Gen $ \k -> unGen ma $ \a -> k (f a)

instance Applicative Gen where
  pure a = Gen $ \k -> k a
  (<*>) gf ga = Gen $ \k -> unGen gf $ \f -> unGen ga $ \a -> k (f a)

instance Monad Gen where
  return = pure
  (>>=) ga f = Gen $ \k -> unGen ga $ \a -> unGen (f a) k

instance MonadFail Gen where
  fail = error

class Monad m => MonadGen m where
  liftGen :: Gen a -> m a

instance MonadGen Gen where
  liftGen = id

instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ReaderT r m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ExceptT e m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (MaybeT m) where
  liftGen = lift . liftGen

genLet :: MonadGen m => CodeQ a -> m (CodeQ a)
genLet a = liftGen $ Gen $ \k -> [|| let x = $$a in seq x $$(k [||x||]) ||]

genSpread :: MonadGen m => CodeQ (a,b) -> m (CodeQ a, CodeQ b)
genSpread cab = liftGen $ Gen $ \k' -> [||
    let (a,b) = $$cab in
    $$(k' ([||a||],[||b||]))
 ||]

genIf :: CodeQ Bool -> Gen a -> Gen a -> Gen a
genIf cb (Gen x) (Gen y) = Gen $ \k -> [||
    if $$cb then $$(x k) else $$(y k)
 ||]

genLetRec :: MonadGen m => (CodeQ a -> CodeQ a) -> m (CodeQ a)
genLetRec a = liftGen $ Gen $ \k -> [|| let x = $$(a [||x||]) in $$(k [||x||]) ||]
