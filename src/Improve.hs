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

class MonadGen n => Improve m n | m -> n, n -> m where
  up   :: CodeQ (m a) -> n (CodeQ a)
  down :: n (CodeQ a) -> CodeQ (m a)

instance Improve Identity Gen where
  up x = gen \k -> k [||runIdentity $$x||]
  down x = unGen x \a -> [||Identity $$a||]

{- YUCK! -}
data IOGen a = IOGen (CodeQ (State# RealWorld) -> GenRep (TupleRep ((:) @RuntimeRep ZeroBitRep ((:) @RuntimeRep LiftedRep ('[] @RuntimeRep)))) (CodeQ (State# RealWorld),Gen a))

instance Functor IOGen where
instance Applicative IOGen where
instance Monad IOGen where
instance MonadGen IOGen where
  liftGen ga = IOGen (return . (,ga))

instance Improve IO IOGen where
  up cioa = IOGen (\csr -> GenRep \k -> [||
        let (IO f) = $$cioa in
        let (# csr', a #) = f $$csr in
        $$(k ([|| csr' ||],return [||a||]))
     ||]
    )
  down (IOGen k) = [||
    IO (\sr -> $$(runGenRep $ do {
      (csr,q) <- k [||sr||];
      return [|| (# $$csr, $$(runGen q) #) ||]
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