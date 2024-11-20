
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


module UnStaged where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Control.Monad

data Step m s a where
    Done :: Step m s a
    Eff :: m s -> Step m s a
    Skip :: s -> Step m s a
    Yield :: a -> s -> Step m s a

data Stream m a where
    S :: forall m a s. s -> (s -> Step m s a) -> Stream m a 

{-
1. With this type, write the stream combinators: take,drop,map, zip...
2. Write effectful combinators: repeatM :: m a -> Stream m a, more!
3. Write an actual effectful stream: Stream IO Int, which produces a stream of ints read from the terminal.
4. WRite "good program" with this, and ensure it works. sum (take 3 (map read (repeatM getLine)))
5. Stage it.
-}

{-
Code Q a
is a piece of haskell code, that if you run it, evaluates to a value of type a.
-}

{-
data Step m s a where
    Done :: Step m s a
    Eff :: Code Q (m s) -> Step m s a
    Skip :: Code Q s -> Step m s a
    Yield :: Code Q a -> Code Q s -> Step m s a

data Stream m a where
    S :: forall m a s. Code Q s -> (Code Q s -> Step m s a) -> Stream m a 
-}