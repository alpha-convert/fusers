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
    Return :: Code Q a -> Action m a
    Bind :: Code Q (m a) -> (Code Q a -> Action m b) -> Action m b

embed :: Code Q (m a) -> Action m a
embed cma = Bind cma Return

bindAction :: Action m a -> (Code Q a -> Action m b) -> Action m b
bindAction (Return x) k = k x
bindAction (Bind cma k) k' = Bind cma (\x -> bindAction (k x) k')

flatten :: Monad m => Action m a -> Code Q (m a)
flatten (Return ca) = [|| return $$ca ||]
flatten (Bind ca k) = [|| $$ca >>= (\a -> $$(flatten (k [||a||]))) ||]