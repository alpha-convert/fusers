
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module GenRep where
import Language.Haskell.TH
import GHC.Base

{-
Mostly based on:
https://github.com/AndrasKovacs/staged/blob/main/icfp24paper/supplement/haskell-cftt/CFTT/Gen.hs
with some extra representation polymorphism.
-}

newtype GenRep (r :: RuntimeRep) a = GenRep {unGenRep :: forall z. (a -> CodeQ @r z) -> CodeQ @r z}

runGenRep :: forall r a. GenRep r (CodeQ @r a) -> CodeQ @r a
runGenRep (GenRep f) = f id


instance Functor (GenRep r) where
  fmap f ma = GenRep $ \k -> unGenRep ma $ \a -> k (f a)

instance Applicative (GenRep r) where
  pure a = GenRep $ \k -> k a
  (<*>) gf ga = GenRep $ \k -> unGenRep gf $ \f -> unGenRep ga $ \a -> k (f a)

instance Monad (GenRep r) where
  return = pure
  (>>=) ga f = GenRep $ \k -> unGenRep ga $ \a -> unGenRep (f a) k

