{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Stream where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Control.Monad
import Gen
import Split
import Improve
import Data.Void

{-
We need to win on all of these benchmarks.
https://github.com/composewell/streaming-benchmarks/tree/master?tab=readme-ov-file
-}


{- States are a heterogenous list of statically-known state types.
Doing this ensures we don't have to box/unbox state tuples each time we go through the loop of a fold.
The Oleg et al paper goes directly to doing imperative code to avoid this, but we can do better with fancy types :)
-}
data States l where
    SNil :: States '[]
    SCons :: CodeQ s -> States l -> States (s ': l)

ssing :: CodeQ a -> States (a ': '[])
ssing ca = SCons ca SNil

data Step a n r l where
    Effect :: n (States l) -> Step a n r l
    Tau :: States l -> Step a n r l
    Yield :: CodeQ a -> States l -> Step a n r l
    Done :: CodeQ r -> Step a n r l

stateMap :: Functor n => (States l -> States l') -> Step a n r l -> Step a n r l'
stateMap f (Effect cmx) = Effect (f <$> cmx)
stateMap f (Tau cx) = Tau (f cx)
stateMap f (Yield ca cx) = Yield ca (f cx)
stateMap _ (Done cr) = Done cr


data Stream a n r where
    {- ideally we want this to be multisteps! -}
    S :: forall a n r l. States l -> (States l -> Gen (Step a n r l)) -> Stream a n r

{-
CONSTRUCTION
-}

range :: (Lift a, Enum a, Ord a) => a -> a -> Stream a m ()
range lo hi = S (ssing [|| lo ||]) $ \(SCons cn _) -> Gen $ \k -> [||
    if $$cn >= hi then $$(k (Done [|| () ||]))
    else $$(k (Yield [|| $$cn ||] (ssing [|| succ $$cn ||])))
 ||]

repeat :: (Improve m n) => CodeQ (m a) -> Stream a n Void
repeat act = S (ssing [|| Nothing ||]) $ \(SCons cmaybea _) -> do
    maybea <- split cmaybea
    case maybea of
        Nothing -> return $ Effect $ do
            a <- up act
            return (ssing [|| Just $$a ||])
        Just ca -> return (Yield ca (ssing [|| Nothing ||]))

{-
COMBINATORS
-}

mapC :: (CodeQ a -> CodeQ b) -> Stream a m r -> Stream b m r
mapC f (S cx0 next) = S cx0 $ \cx -> do
    next cx >>= \case
        Effect ams -> return (Effect ams)
        Tau cx' -> return (Tau cx')
        Yield ca cx' -> return (Yield (f ca) cx')
        Done cr -> return (Done cr)

map :: CodeQ (a -> b) -> Stream a m r -> Stream b m r
map f = mapC (\ca -> [|| $$f $$ca ||])

drop :: (Functor m) => Int -> Stream a m r -> Stream a m r
drop n (S cx0 next) = S (SCons [|| n ||] cx0) $ \(SCons cn cx)-> do
    b <- split [|| $$cn <= 0 ||]
    if b then stateMap (SCons [||0||]) <$> next cx else do
      next cx >>= \case
          Effect cmx' -> return (Effect (SCons cn <$> cmx'))
          Tau cx' -> return (Tau (SCons cn cx'))
          Yield _ cx' -> return (Tau (SCons [|| $$cn - 1 ||] cx') )
          Done cr -> return (Done cr)

dropWhileC :: (Improve m n) => (CodeQ a -> CodeQ Bool) -> Stream a n r -> Stream a n r
dropWhileC f (S cx0 next) = S (SCons [||True||] cx0) $ \(SCons cb cx) -> do
    b <- split cb
    if not b
    then stateMap (SCons [||False||]) <$> next cx
    else next cx >>= \case
            -- Effect cmx' -> return (Effect (fmapAction (with True) cmx'))
            Effect cmx' -> return (Effect (SCons [||True||] <$> cmx'))
            Tau cx' -> return (Tau (SCons [||True||] cx'))
            Yield ca cx' -> do
                b' <- split (f ca)
                if b' then return (Tau (SCons [|| True ||] cx')) else return (Yield ca (SCons [||False||] cx'))
            Done cr -> return (Done cr)

take :: Functor m => Int -> Stream a m r -> Stream a m r
take n0 (S cx0 next) = S (SCons [|| n0 ||] cx0) $ \(SCons cn cx) -> Gen $ \k ->
    [||
        if $$cn > 0 then
        $$(unGen (next cx) (\case
            Effect cmx' -> k (Effect (SCons cn <$> cmx'))
            Tau cx' -> k (Tau (SCons cn cx'))
            Yield ca cx' -> k (Yield ca (SCons [|| $$cn - 1 ||] cx'))
            Done cr -> k (Done cr)
        ))
        else $$(unGen (next cx) (\case
            Effect cmx' -> k (Effect (SCons [||0||] <$> cmx'))
            Tau cx' -> k (Tau (SCons [||0||] cx'))
            Yield _ cx' -> k (Tau (SCons [||0||] cx'))
            Done cr -> k (Done cr)
        ))
    ||]

takeWhileC :: Functor m => (CodeQ a -> CodeQ Bool) -> Stream a m r -> Stream a m ()
takeWhileC f (S cx0 next) = S (SCons [||True||] cx0) $ \(SCons cb cx) -> Gen $ \k -> [||
        if not $$cb then $$(k (Done [|| () ||])) else $$(unGen (next cx) (\case
            Effect cmx' -> k (Effect (SCons [||True||] <$> cmx'))
            Tau cx' -> k (Tau (SCons [||True||] cx'))
            Yield ca cx' -> [|| if $$(f ca) then $$(k (Yield ca (SCons [||True||] cx'))) else $$(k (Tau (SCons [||False||] cx'))) ||]
            Done _ -> k (Done [|| () ||])
        ))
    ||]

filterC :: (CodeQ a -> CodeQ Bool) -> Stream a m r -> Stream a m r
filterC p (S cx0 next) = S cx0 $ \cx -> do
    st <- next cx
    case st of
        Effect cmx' -> return (Effect cmx')
        Tau cx' -> return (Tau cx')
        Yield ca cx' -> do
            b <- split (p ca)
            if b then return (Yield ca cx') else return (Tau cx')
        Done cr -> return (Done cr)

filter :: CodeQ (a -> Bool) -> Stream a m r -> Stream a m r
filter f = filterC (\ca -> [|| $$f $$ca ||])

scanC :: Functor m => (CodeQ x -> CodeQ a -> CodeQ x) -> CodeQ x -> (CodeQ x -> CodeQ b) -> Stream a m r -> Stream b m r
scanC step begin done (S cs0 next) = S (SCons begin cs0) $ \(SCons cx cs) -> do
    next cs >>= \case
        Effect cms' -> return (Effect (SCons cx <$> cms'))
        Tau cs' -> return (Tau (SCons cx cs'))
        Yield ca cs' -> do
            cx' <- genLet (step cx ca)
            return (Yield (done cx') (SCons cx' cs'))
        Done cr -> return (Done cr)

{-
ELIMINATORS
-}
foldC :: (Monad m, Improve m n)  => (CodeQ x -> CodeQ a -> CodeQ x) -> CodeQ x -> (CodeQ x -> CodeQ b) -> Stream a n r -> CodeQ (m (b,r))
foldC = undefined
-- foldC step begin done (S cx0 next) = [|| do
--     let loop !acc !x = $$(unGen (next [|| x ||]) $ \case
--             Effect cmx' -> [|| $$(down cmx') >>= loop acc ||]
--             Tau cx' -> [|| loop acc $$cx' ||]
--             Yield ca cx' -> [|| loop $$(step [|| acc ||] ca) $$cx' ||]
--             Done cr -> [|| return ($$(done [|| acc ||]),$$cr) ||]
--          )
--     loop $$begin $$cx0
--  ||]


toListC :: (Monad m, Improve m n) => Stream a n r -> CodeQ (m ([a],r))
toListC = foldC (\cdl ca -> [|| \ls -> $$cdl ($$ca : ls) ||]) [|| id ||] (\dl -> [|| $$dl [] ||])

sumC :: (Monad m, Improve m n) => Stream Int n r -> CodeQ (m Int)
sumC s = [|| $$(foldC (\cx cy -> [|| $$cx + $$cy ||]) [|| 0 ||] (\cx -> [|| return $$cx ||]) s) >>= fst ||]

fromListC :: CodeQ [a] -> Stream a m ()
fromListC cxs0 = S (ssing cxs0) $ \(SCons cxs _) -> Gen $ \k -> [||
    case $$cxs of
        [] -> $$(k (Done [|| () ||]))
        y:ys -> $$(k (Yield [|| y ||] (ssing [||ys||])))
  ||]

end :: (Monad m, Improve m n) => Stream a n r -> CodeQ (m r)
end s = [|| (snd <$> $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s)) ||]

drain :: (Monad m, Improve m n) => Stream a n r -> CodeQ (m ())
drain s = [|| (Control.Monad.void $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s))||]