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
{-# LANGUAGE BangPatterns #-}

module ImpStream where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
-- import GHC.STRef (STRef(..), newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Trans
import qualified Control.Monad.Trans.Class as MT


data Step a m r where
    Effect :: Code Q (m ()) -> Step a m r
    Tau :: Step a m r
    Yield :: Code Q a -> Step a m r
    Done :: Code Q r -> Step a m r

-- stateMapC :: Functor m => (Code Q s -> Code Q s') -> Step a m r s -> Step a m r s'
-- stateMapC f (Effect cmx) = Effect [|| fmap (\x -> $$(f [|| x ||])) $$cmx ||]
-- stateMapC f (Tau cx) = Tau (f cx)
-- stateMapC f (Yield ca cx) = Yield ca (f cx)
-- stateMapC _ (Done cr) = Done cr

data Stream_ a m r t where
    S :: forall a m r s t.
        (forall w . (s -> Code Q (STT t m w)) -> Code Q (STT t m w)) ->
        (s -> (forall w. (Step a m r -> Code Q (STT t m w)) -> Code Q (STT t m w))) ->
        Stream_ a m r t

type Stream a m r = forall t. Stream_ a m r t

mapC :: (Code Q a -> Code Q b) -> Stream a m r -> Stream b m r
mapC f (S init next) = S init $ \x k -> next x (\case
        Effect m -> k (Effect m)
        Tau -> k Tau
        Yield ca -> k (Yield (f ca))
        Done cr -> k (Done cr)
    )
map :: Code Q (a -> b) -> Stream a m r -> Stream b m r
map f = mapC (\ca -> [|| $$f $$ca ||])

drop :: (Monad m) => Int -> Stream a m r -> Stream a m r
drop n (S init next) = S (\k -> init (\x0 -> [|| do {n_ref <- newSTRef n; $$(k ([|| n_ref ||],x0))} ||])) $ \(cnref,x) k -> [||
    do {
        n <- readSTRef $$cnref;
        if n <= 0 then $$(next x k) else
        $$(next x (\case
            Effect m -> k (Effect m)
            Tau -> k Tau
            Yield _ -> [|| writeSTRef $$cnref (n-1) >> $$(k Tau) ||]
            Done r -> k (Done r)
        ))
    }
 ||]

filterC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m r
filterC p (S init next) = S init $ \x k -> next x (\case
    Effect cm -> k (Effect cm)
    Tau -> k Tau
    Yield ca -> [|| if $$(p ca) then $$(k (Yield ca)) else $$(k Tau) ||]
    Done cr -> k (Done cr)
 )

-- filterC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m r
-- filterC p (S cx0 next) = S cx0 $ \cx k ->
--     next cx (\case
--         Effect cmx' -> k (Effect cmx')
--         Tau cx' -> k (Tau cx')
--         Yield ca cx' -> [||
--             if $$(p ca) then $$(k (Yield ca cx')) else $$(k (Tau cx'))
--          ||]
--         Done cr -> k (Done cr)
--     )

-- filter :: Code Q (a -> Bool) -> Stream a m r -> Stream a m r
-- filter f = filterC (\ca -> [|| $$f $$ca ||])


foldCST :: (Monad m, Lift x) => (Code Q x -> Code Q a -> Code Q x) -> Code Q x -> (Code Q x -> Code Q b) -> Stream a m r -> Code Q (STT t m (b,r))
foldCST step begin done (S init next) = init $ \x -> [|| do
    r_acc <- newSTRef $$begin
    let loop = $$(next x (\case {
        Effect m -> [|| MT.lift $$m >> loop ||];
        Tau -> [||loop||];
        Yield ca -> [|| do {acc <- readSTRef r_acc; writeSTRef r_acc $$(step [|| acc ||] ca); loop} ||];
        Done cr -> [|| do {acc <- readSTRef r_acc; return ($$(done [|| acc ||]),$$cr)} ||]
        }))
    loop
  ||]
--     let loop !acc !x = $$(next [|| x ||] $ \case
--             Effect cmx' -> [|| $$cmx' >>= loop acc ||]
--             Tau cx' -> [|| loop acc $$cx' ||]
--             Yield ca cx' -> [|| loop $$(step [|| acc ||] ca) $$cx' ||]
--             Done cr -> [|| return ($$(done [|| acc ||]),$$cr) ||]
--          )
--     loop $$begin $$cx0
--  ||]

foldC :: (Lift x, Monad m) => (Code Q x -> Code Q a -> Code Q x) -> Code Q x -> (Code Q x -> Code Q b) -> Stream a m r -> Code Q (m (b,r))
foldC step begin done s = [|| runSTT $$(foldCST step begin done s) ||]

-- toListC :: Monad m => Stream a m r -> Code Q (m ([a],r))
-- toListC = foldC (\cdl ca -> [|| \ls -> $$cdl ($$ca : ls) ||]) [|| id ||] (\dl -> [|| $$dl [] ||])

sumC :: Monad m => Stream Int m r -> Code Q (m Int)
sumC s = [|| fst <$> $$(foldC (\cx cy -> [|| $$cx + $$cy ||]) [|| 0 ||] (\cx -> [|| $$cx ||]) s)||]

-- fromListC :: Code Q [a] -> Stream a m ()
-- fromListC cxs0 = S cxs0 $ \cxs k -> [||
--     case $$cxs of
--         [] -> $$(k (Done [|| () ||]))
--         y:ys -> $$(k (Yield [|| y ||] [||ys||]))
--   ||]

range :: (Monad m, Lift a, Enum a, Ord a) => a -> a -> Stream a m ()
range lo hi = S (\k -> [|| do {n <- newSTRef lo; $$(k [|| n ||])} ||]) $ \n_ref k -> [|| do
    n <- readSTRef $$n_ref
    if n >= hi then $$(k (Done [|| () ||])) else writeSTRef $$n_ref (succ n) >> $$(k (Yield [|| n ||]))
    --  if $$cn >= hi then $$(k (Done [|| () ||]))
    --  else $$(k (Yield [|| $$cn ||] [|| succ $$cn ||]))
  ||]