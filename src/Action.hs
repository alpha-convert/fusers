{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Action where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- normalized monadic form: all of the binds are fully normal static chains.
-- The control flow is completely statically determined??
-- First level of the monad gets interpreted away staticaly.
data Action m a where
    Return :: CodeQ a -> Action m a
    Bind :: CodeQ (m a) -> (CodeQ a -> Action m b) -> Action m b

embed :: CodeQ (m a) -> Action m a
embed cma = Bind cma Return

fmapAction :: (CodeQ a -> CodeQ b) -> Action m a -> Action m b
fmapAction f (Return x) = Return (f x)
fmapAction f (Bind cma k) = Bind cma (fmapAction f . k)

bindAction :: Action m a -> (CodeQ a -> Action m b) -> Action m b
bindAction (Return x) k = k x
bindAction (Bind cma k) k' = Bind cma (\x -> bindAction (k x) k')

flatten :: Monad m => Action m a -> CodeQ (m a)
flatten (Return ca) = [|| return $$ca ||]
flatten (Bind cma k) = [|| $$cma >>= (\a -> $$(flatten (k [||a||]))) ||]