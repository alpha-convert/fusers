{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Gen (
  Gen(..),
  MonadGen(..),
  gen,
  unGen,
  runGen,
  genLet,
  genLetRec
)
where
import GenRep
import Language.Haskell.TH
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import GHC.Base
import Control.Monad.Trans

{-
Gen is just:
data Gen a = Gen (forall z. (a -> CodeQ z) -> CodeQ z)
But we need the representation-polymorphic version for fusing IO.
-}
newtype Gen a = Gen (GenRep LiftedRep a)

gen :: (forall z. (a -> CodeQ @LiftedRep z) -> CodeQ @LiftedRep z) -> Gen a
gen k = Gen (GenRep k)

unGen ::  Gen a -> (forall z. (a -> CodeQ @LiftedRep z) -> CodeQ @LiftedRep z)
unGen (Gen (GenRep k)) = k

instance Functor Gen where
  fmap f (Gen g) = Gen (f <$> g)
instance Applicative Gen where
  pure = return
  liftA2 = liftM2

instance Monad Gen where
  return x = Gen (return x)
  (>>=) (Gen x) f = Gen (x >>= ((\(Gen u) -> u) . f))


instance MonadFail (GenRep r) where
  fail = error

class Monad m => MonadGen m where
  liftGen :: Gen a -> m a

instance MonadGen Gen where
  liftGen = id


-- newtype Gen a = Gen {unGen :: forall r. (a -> CodeQ r) -> CodeQ r}
runGen :: Gen (CodeQ a) -> CodeQ a
runGen (Gen (GenRep f)) = f id

instance MonadGen m => MonadGen (StateT s m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ReaderT r m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (ExceptT e m) where
  liftGen = lift . liftGen

instance MonadGen m => MonadGen (MaybeT m) where
  liftGen = lift . liftGen


genLet :: MonadGen m => CodeQ a -> m (CodeQ a)
genLet a = liftGen $ Gen $ GenRep $ \k -> [|| let x = $$a in seq x $$(k [||x||]) ||]

genLetRec :: MonadGen m => (CodeQ a -> CodeQ a) -> m (CodeQ a)
genLetRec a = liftGen $ Gen $ GenRep $ \k -> [|| let x = $$(a [||x||]) in $$(k [||x||]) ||]
