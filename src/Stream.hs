{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Stream where

data Step a m r s where
    Effect :: m s -> Step a m r s
    Tau :: s -> Step a m r s
    Yield :: a -> s -> Step a m r s
    Done :: r -> Step a m r s

data Stream a m r where
    S :: forall a m r s. s -> (s -> Step a m r s) -> Stream a m r