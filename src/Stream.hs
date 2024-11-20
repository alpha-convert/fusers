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
import Action
import Gen

{-
We need to win on all of these benchmarks.
https://github.com/composewell/streaming-benchmarks/tree/master?tab=readme-ov-file
-}

{-
Even better: we should probably fuse the monads themselves, too!
https://github.com/AndrasKovacs/staged/tree/main/icfp24paper/supplement
-}
data Step a m r s where
    Effect :: Action m s -> Step a m r s
    Tau :: Code Q s -> Step a m r s
    Yield :: Code Q a -> Code Q s -> Step a m r s
    Done :: Code Q r -> Step a m r s

stateMapC :: (Code Q s -> Code Q s') -> Step a m r s -> Step a m r s'
stateMapC f (Effect cmx) = Effect (fmapAction f cmx)
stateMapC f (Tau cx) = Tau (f cx)
stateMapC f (Yield ca cx) = Yield ca (f cx)
stateMapC _ (Done cr) = Done cr

data Stream a m r where
    {- State should probably be an HList of statically known states. -}
    S :: forall a m r s. Code Q s -> (Code Q s -> Gen (Step a m r s)) -> Stream a m r

{-
CONSTRUCTION
-}

range :: (Lift a, Enum a, Ord a) => a -> a -> Stream a m ()
range lo hi = S [|| lo ||] $ \cn -> Gen $ \k -> [||
    if $$cn >= hi then $$(k (Done [|| () ||]))
    else $$(k (Yield [|| $$cn ||] [|| succ $$cn ||]))
 ||]



{-
COMBINATORS
-}

mapC :: (Code Q a -> Code Q b) -> Stream a m r -> Stream b m r
mapC f (S cx0 next) = S cx0 $ \cx -> do
    next cx >>= \case
        Effect ams -> return (Effect ams)
        Tau cx -> return (Tau cx)
        Yield ca cx -> return (Yield (f ca) cx)
        Done cr -> return (Done cr)

map :: Code Q (a -> b) -> Stream a m r -> Stream b m r
map f = mapC (\ca -> [|| $$f $$ca ||])

drop :: Int -> Stream a m r -> Stream a m r
drop n (S cx0 next) = S [|| ($$cx0,n) ||] $ \cxn -> do
    (cx,cn) <- genSpread cxn
    genIf [|| $$cn <= 0 ||] (stateMapC andZero <$> next cx) $ do
        next cx >>= \case
            Effect cmx' -> return (Effect (fmapAction (\cs -> [|| ($$cs,$$cn) ||]) cmx'))
            Tau cx' -> return (Tau [|| ($$cx',$$cn) ||])
            Yield _ cx' -> return (Tau [|| ($$cx',$$cn - 1) ||])
            Done cr -> return (Done cr)
         where
             andZero cx = [|| ($$cx,0) ||]

dropWhileC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m r
dropWhileC f (S cx0 next) = S [|| ($$cx0,True) ||] $ \cxb -> do
    (cx,cb) <- genSpread cxb
    genIf [|| not $$cb ||] (stateMapC (with False) <$> next cx) $
        next cx >>= \case
            Effect cmx' -> return (Effect (fmapAction (with True) cmx'))
            Tau cx' -> return (Tau [||($$cx',True)||])
            Yield ca cx' -> genIf (f ca) (return (Tau (with True cx'))) (return (Yield ca (with False cx')))
            Done cr -> return (Done cr)
        where
            with b cx = [|| ($$cx,b) ||]

take :: Int -> Stream a m r -> Stream a m r
take n (S cx0 next) = S [|| ($$cx0,n) ||] $ \cxn -> Gen $ \k ->
    [||
        let !(x,n) = $$cxn in
        if n > 0 then
        $$(unGen (next [||x||]) (\case
            Effect cmx' -> k (Effect (fmapAction (\cs -> [|| ($$cs,n) ||]) cmx'))
            Tau cx' -> k (Tau [|| ($$cx',n) ||])
            Yield ca cx' -> k (Yield ca [|| ($$cx',n-1) ||])
            Done cr -> k (Done cr)
        ))
        else $$(unGen (next [||x||]) (\case
            Effect cmx' -> k (Effect (fmapAction andZero cmx'))
            Tau cx' -> k (Tau (andZero cx'))
            Yield _ cx' -> k (Tau (andZero cx'))
            Done cr -> k (Done cr)
        ))
    ||]
        where
            andZero cx = [|| ($$cx,0) ||]

takeWhileC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m ()
takeWhileC f (S cx0 next) = S [|| ($$cx0,True) ||] $ \cx -> Gen $ \k -> [||
        let !(x,b) = $$cx in
        if not b then $$(k (Done [|| () ||])) else $$(unGen (next [||x||]) (\case
            Effect cmx' -> k (Effect (fmapAction (with True) cmx'))
            Tau cx' -> k (Tau (with True cx'))
            Yield ca cx' -> [|| if $$(f ca) then $$(k (Yield ca (with True cx'))) else $$(k (Tau (with False cx'))) ||]
            Done _ -> k (Done [|| () ||])
        ))
    ||]
        where
            with b cx = [|| ($$cx,b) ||]

filterC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m r
filterC p (S cx0 next) = S cx0 $ \cx ->
    next cx >>= \case
        Effect cmx' -> return (Effect cmx')
        Tau cx' -> return (Tau cx')
        Yield ca cx' -> genIf (p ca) (return (Yield ca cx')) (return (Tau cx'))
        Done cr -> return (Done cr)

filter :: Code Q (a -> Bool) -> Stream a m r -> Stream a m r
filter f = filterC (\ca -> [|| $$f $$ca ||])

scanC :: (Code Q x -> Code Q a -> Code Q x) -> Code Q x -> (Code Q x -> Code Q b) -> Stream a m r -> Stream b m r
scanC step begin done (S cs0 next) = S [|| ($$cs0,$$begin) ||] $ \csx -> do
    (cs,cx) <- genSpread csx 
    next cs >>= \case
        Effect cms' -> return (Effect (fmapAction (\cs' -> [|| ($$cs',$$cx) ||]) cms'))
        Tau cs' -> return (Tau [|| ($$cs',$$cx)||])
        Yield ca cs' -> do
            cx' <- genLet (step cx ca)
            return (Yield (done cx') [|| ($$cs',$$cx') ||])
        Done cr -> return (Done cr)

{-
ELIMINATORS
-}
foldC :: Monad m => (Code Q x -> Code Q a -> Code Q x) -> Code Q x -> (Code Q x -> Code Q b) -> Stream a m r -> Code Q (m (b,r))
foldC step begin done (S cx0 next) = [|| do
    let loop !acc !x = $$(unGen (next [|| x ||]) $ \case
            Effect cmx' -> [|| $$(flatten cmx') >>= loop acc ||]
            Tau cx' -> [|| loop acc $$cx' ||]
            Yield ca cx' -> [|| loop $$(step [|| acc ||] ca) $$cx' ||]
            Done cr -> [|| return ($$(done [|| acc ||]),$$cr) ||]
         )
    loop $$begin $$cx0
 ||]


toListC :: Monad m => Stream a m r -> Code Q (m ([a],r))
toListC = foldC (\cdl ca -> [|| \ls -> $$cdl ($$ca : ls) ||]) [|| id ||] (\dl -> [|| $$dl [] ||])

sumC :: Monad m => Stream Int m r -> Code Q (m Int)
sumC s = [|| $$(foldC (\cx cy -> [|| $$cx + $$cy ||]) [|| 0 ||] (\cx -> [|| return $$cx ||]) s) >>= fst ||]

fromListC :: Code Q [a] -> Stream a m ()
fromListC cxs0 = S cxs0 $ \cxs -> Gen $ \k -> [||
    case $$cxs of
        [] -> $$(k (Done [|| () ||]))
        y:ys -> $$(k (Yield [|| y ||] [||ys||]))
  ||]


end :: Monad m => Stream a m r -> Code Q (m r)
end s = [|| (snd <$> $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s)) ||]

drain :: Monad m => Stream a m r -> Code Q (m ())
drain s = [|| (Control.Monad.void $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s))||]