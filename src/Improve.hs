{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TupleSections#-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}


{-
This is more or less entirely taken from
https://github.com/AndrasKovacs/staged/blob/main/icfp24paper/supplement/haskell-cftt/CFTT/Improve.hs.
-}

module Improve where
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import GenRep
import Gen
import Language.Haskell.TH
import Data.Functor.Identity
import Control.Monad.Trans.State
import Split
import Control.Monad.ST (RealWorld)
import GHC.Base
import Unsafe.Coerce

class MonadGen n => Improve m n | m -> n, n -> m where
  up   :: CodeQ (m a) -> n (CodeQ a)
  down :: n (CodeQ a) -> CodeQ (m a)

instance Improve Identity Gen where
  up x = gen \k -> k [||runIdentity $$x||]
  down x = unGen x \a -> [||Identity $$a||]

{-
IO = State (State# RealWorld)
=>
Improve IO = Improve (StateT (State# RealWorld) Identity)
           = (StateT (CodeQ (State# RealWorld)) (Improve Identity))
           = StateT (CodeQ (State# RealWorld)) Gen

-}

{- YUCK! -}
data IOGen a = IOGen (CodeQ (State# RealWorld) -> GenRep (TupleRep ((:) @RuntimeRep ZeroBitRep ((:) @RuntimeRep LiftedRep ('[] @RuntimeRep)))) (CodeQ (State# RealWorld),a))

instance Functor IOGen where
  fmap = liftM

instance Applicative IOGen where
  pure = return
  liftA2 = liftM2

instance Monad IOGen where
  return x = IOGen (\csr -> return (csr, x))
  (>>=) (IOGen f) g = IOGen \csr ->
    let (GenRep k) = f csr in
    GenRep (\k' -> k (\(csr',a) ->
        let (IOGen u) = g a in
        unGenRep (u csr') k'
      ))

instance MonadGen IOGen where
  liftGen (Gen (GenRep k')) = IOGen (\csr -> GenRep \k -> unsafeCoerce $ k' (\a -> unsafeCoerce $ k (csr,a)))

instance Improve IO IOGen where
  up cioa = IOGen (\csr -> GenRep \k -> [||
        let (IO f) = $$cioa in
        let (# csr', a #) = f $$csr in
        $$(k ([|| csr' ||],[||a||]))
     ||]
    )
  down (IOGen k) = [||
    IO (\sr -> $$(runGenRep $ do {
      (csr,q) <- k [||sr||];
      return [|| (# $$csr, $$q #) ||]
    })) ||]

instance (Improve m n) => Improve (StateT s m) (StateT (CodeQ s) n) where

  up x = StateT \s ->
    do as <- up [|| runStateT $$x $$s ||]
       split as

  down x = [|| StateT \s -> $$(down
    do (a, s) <- runStateT x [||s||]
       pure [||($$a, $$s)||]
       )||]

instance (Improve m n) => Improve (ExceptT e m) (ExceptT (CodeQ e) n) where
  up x = ExceptT do
    ea <- up [||runExceptT $$x||]
    split ea

  down (ExceptT x) =
    [|| ExceptT $$(down (x >>= \case
          Left e  -> pure [||Left $$e||]
          Right a -> pure [||Right $$a||]
                        )) ||]

instance Improve m n => Improve (MaybeT m) (MaybeT n) where
  up x = MaybeT do
    ma <- up [||runMaybeT $$x||]
    split ma

  down (MaybeT x) =
    [|| MaybeT $$(down (x >>= \case
          Nothing -> pure [||Nothing||]
          Just a -> pure [||Just $$a||])) ||]

instance Improve m n => Improve (ReaderT r m) (ReaderT (CodeQ r) n) where
  up   x = ReaderT \r -> up [||runReaderT $$x $$r||]
  down x = [|| ReaderT \r -> $$(down (runReaderT x [||r||])) ||]