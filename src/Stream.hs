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

{-
Even better: we should probably fuse the monads themselves, too!
https://github.com/AndrasKovacs/staged/tree/main/icfp24paper/supplement
-}
data Step a n r s where
    Effect :: n (Code Q s) -> Step a n r s
    Tau :: CodeQ s -> Step a n r s
    Yield :: CodeQ a -> CodeQ s -> Step a n r s
    Done :: CodeQ r -> Step a n r s

stateMapC :: Functor n => (CodeQ s -> CodeQ s') -> Step a n r s -> Step a n r s'
stateMapC f (Effect cmx) = Effect (f <$> cmx)
stateMapC f (Tau cx) = Tau (f cx)
stateMapC f (Yield ca cx) = Yield ca (f cx)
stateMapC _ (Done cr) = Done cr

data Stream a n r where
    S :: forall a n r s. Gen (CodeQ s) -> (CodeQ s -> Gen (Step a n r s)) -> Stream a n r

{-
CONSTRUCTION
-}

range :: (Lift a, Enum a, Ord a) => a -> a -> Stream a m ()
range lo hi = S (return [|| lo ||]) $ \cn -> gen $ \k -> [||
    if $$cn >= hi then $$(k (Done [|| () ||]))
    else $$(k (Yield [|| $$cn ||] [|| succ $$cn ||]))
 ||]

repeat :: (Improve m n) => CodeQ (m a) -> Stream a n Void
repeat act = S (return [|| Nothing ||]) $ \cmaybea -> do
    maybea <- split cmaybea
    case maybea of
        Nothing -> return $ Effect $ do
            a <- up act
            return [|| Just $$a ||]
        Just ca -> return (Yield ca [|| Nothing ||])

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
drop n (S kcx0 next) = S (do {cx0 <- kcx0; return [|| ($$cx0,n) ||]}) $ \cxn -> do
    (cx,cn) <- split cxn
    b <- split [|| $$cn <= 0 ||]
    if b then stateMapC andZero <$> next cx else do
      next cx >>= \case
          Effect cmx' -> return (Effect ((\cx' -> [|| ($$cx',$$cn) ||] ) <$> cmx'))
          Tau cx' -> return (Tau [|| ($$cx',$$cn) ||])
          Yield _ cx' -> return (Tau [|| ($$cx',$$cn - 1) ||])
          Done cr -> return (Done cr)
       where
           andZero cx = [|| ($$cx,0) ||]

dropWhileC :: (Improve m n) => (CodeQ a -> CodeQ Bool) -> Stream a n r -> Stream a n r
dropWhileC f (S kcx0 next) = S (do {cx0 <- kcx0; return [|| ($$cx0,True) ||]}) $ \cxb -> do
    (cx,cb) <- split cxb
    b <- split cb
    if not b
    then stateMapC (with False) <$> next cx
    else next cx >>= \case
            -- Effect cmx' -> return (Effect (fmapAction (with True) cmx'))
            Effect cmx' -> return (Effect (with True <$> cmx'))
            Tau cx' -> return (Tau [||($$cx',True)||])
            Yield ca cx' -> do
                b' <- split (f ca)
                if b' then return (Tau (with True cx')) else return (Yield ca (with False cx'))
            Done cr -> return (Done cr)
        where
            with b cx = [|| ($$cx,b) ||]

take :: Functor m => Int -> Stream a m r -> Stream a m r
take n0 (S kcx0 next) = S (do {cx0 <- kcx0; return [|| ($$cx0,n0) ||]}) $ \cxn -> do
    (cx,cn) <- split cxn
    b <- split [|| $$cn > 0 ||]
    if not b then stateMapC andZero <$> next cx
    else _
    {-
    Gen $ \k ->
    [||
        let !(x,n) = $$cxn in
        if n > 0 then
        $$(unGen (next [||x||]) (\case
            Effect cmx' -> k (Effect ((\cs -> [|| ($$cs,n) ||]) <$> cmx'))
            Tau cx' -> k (Tau [|| ($$cx',n) ||])
            Yield ca cx' -> k (Yield ca [|| ($$cx',n-1) ||])
            Done cr -> k (Done cr)
        ))
        else $$(unGen (next [||x||]) (\case
            Effect cmx' -> k (Effect (andZero <$> cmx'))
            Tau cx' -> k (Tau (andZero cx'))
            Yield _ cx' -> k (Tau (andZero cx'))
            Done cr -> k (Done cr)
        ))
    ||]
    -}
        where
            andZero cx = [|| ($$cx,0) ||]

takeWhileC :: Functor m => (CodeQ a -> CodeQ Bool) -> Stream a m r -> Stream a m ()
takeWhileC f (S kcx0 next) = S (do {cx0 <- kcx0; return [|| ($$cx0,True) ||]}) $ \cx -> gen $ \k -> [||
        let !(x,b) = $$cx in
        if not b then $$(k (Done [|| () ||])) else $$(unGen (next [||x||]) (\case
            Effect cmx' -> k (Effect (with True <$> cmx'))
            Tau cx' -> k (Tau (with True cx'))
            Yield ca cx' -> [|| if $$(f ca) then $$(k (Yield ca (with True cx'))) else $$(k (Tau (with False cx'))) ||]
            Done _ -> k (Done [|| () ||])
        ))
    ||]
        where
            with b cx = [|| ($$cx,b) ||]

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
scanC step begin done (S kcs0 next) = S (do {cs0 <- kcs0; return [|| ($$cs0,$$begin) ||]}) $ \csx -> do
    (cs,cx) <- split csx
    next cs >>= \case
        Effect cms' -> return (Effect ((\cs' -> [|| ($$cs',$$cx) ||]) <$> cms'))
        Tau cs' -> return (Tau [|| ($$cs',$$cx)||])
        Yield ca cs' -> do
            cx' <- genLet (step cx ca)
            return (Yield (done cx') [|| ($$cs',$$cx') ||])
        Done cr -> return (Done cr)

{-
ELIMINATORS
-}
foldC :: (Monad m, Improve m n)  => (CodeQ x -> CodeQ a -> CodeQ x) -> CodeQ x -> (CodeQ x -> CodeQ b) -> Stream a n r -> CodeQ (m (b,r))
foldC step begin done (S kcx0 next) = unGen kcx0 $ \cx0 -> [|| do
    let loop !acc !x = $$(unGen (next [|| x ||]) $ \case
            Effect cmx' -> [|| $$(down cmx') >>= loop acc ||]
            Tau cx' -> [|| loop acc $$cx' ||]
            Yield ca cx' -> [|| loop $$(step [|| acc ||] ca) $$cx' ||]
            Done cr -> [|| return ($$(done [|| acc ||]),$$cr) ||]
         )
    loop $$begin $$cx0
 ||]


toListC :: (Monad m, Improve m n) => Stream a n r -> CodeQ (m ([a],r))
toListC = foldC (\cdl ca -> [|| \ls -> $$cdl ($$ca : ls) ||]) [|| id ||] (\dl -> [|| $$dl [] ||])

sumC :: (Monad m, Improve m n) => Stream Int n r -> CodeQ (m Int)
sumC s = [|| $$(foldC (\cx cy -> [|| $$cx + $$cy ||]) [|| 0 ||] (\cx -> [|| return $$cx ||]) s) >>= fst ||]

fromListC :: CodeQ [a] -> Stream a m ()
fromListC cxs0 = S (return cxs0) $ \cxs -> gen $ \k -> [||
    case $$cxs of
        [] -> $$(k (Done [|| () ||]))
        y:ys -> $$(k (Yield [|| y ||] [||ys||]))
  ||]

end :: (Monad m, Improve m n) => Stream a n r -> CodeQ (m r)
end s = [|| (snd <$> $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s)) ||]

drain :: (Monad m, Improve m n) => Stream a n r -> CodeQ (m ())
drain s = [|| (Control.Monad.void $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s))||]