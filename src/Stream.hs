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

{-
We need to win on all of these benchmarks.
https://github.com/composewell/streaming-benchmarks/tree/master?tab=readme-ov-file
-}

{-
What if we fuse deeper? What if we used a syntactic monad for the embedded
effects, so we could fuse those too? That way some of thse fmaps and binds can
be optimized away...
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
    S :: forall a m r s. Code Q s -> (Code Q s -> (forall w. (Step a m r s -> Code Q w) -> Code Q w)) -> Stream a m r

mapC :: (Code Q a -> Code Q b) -> Stream a m r -> Stream b m r
mapC f (S cx0 next) = S cx0 $ \cx k ->
    next cx (\case
        Effect cmx' -> k (Effect cmx')
        Tau cx' -> k (Tau cx')
        Yield ca cx' -> k (Yield (f ca) cx')
        Done cr -> k (Done cr)
    )

map :: Code Q (a -> b) -> Stream a m r -> Stream b m r
map f = mapC (\ca -> [|| $$f $$ca ||])

drop :: Int -> Stream a m r -> Stream a m r
drop n (S cx0 next) = S [|| ($$cx0,n) ||] $ \cxn k ->
    [||
        let !(x,n) = $$cxn in
        if n > 0 then
        $$(next [||x||] (\case
            Effect cmx' -> k (Effect (fmapAction (\cs -> [|| ($$cs,n) ||]) cmx'))
            Tau cx' -> k (Tau [|| ($$cx',n) ||])
            Yield _ cx' -> k (Tau [|| ($$cx',n-1) ||])
            Done cr -> k (Done cr)
        ))
        else $$(next [||x||] (k . stateMapC andZero))
    ||]
        where
            andZero cx = [|| ($$cx,0) ||]

dropWhileC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m r
dropWhileC f (S cx0 next) = S [|| ($$cx0,True) ||] (\cx k -> [||
        let !(x,b) = $$cx in
        if not b then $$(next [||x||] (k . stateMapC (with False))) else $$(next [||x||] (\case
            Effect cmx' -> k (Effect (fmapAction (with True) cmx'))
            Tau cx' -> k (Tau (with True cx'))
            Yield ca cx' -> [|| if $$(f ca) then $$(k (Tau (with True cx'))) else $$(k (Yield ca (with False cx'))) ||]
            Done cr -> k (Done cr)
        ))
    ||])
        where
            with b cx = [|| ($$cx,b) ||]

take :: Functor m => Int -> Stream a m r -> Stream a m r
take n (S cx0 next) = S [|| ($$cx0,n) ||] $ \cxn k ->
    [||
        let !(x,n) = $$cxn in
        if n > 0 then
        $$(next [||x||] (\case
            Effect cmx' -> k (Effect (fmapAction (\cs -> [|| ($$cs,n) ||]) cmx'))
            Tau cx' -> k (Tau [|| ($$cx',n) ||])
            Yield ca cx' -> k (Yield ca [|| ($$cx',n-1) ||])
            Done cr -> k (Done cr)
        ))
        else $$(next [||x||] (\case
            Effect cmx' -> k (Effect (fmapAction andZero cmx'))
            Tau cx' -> k (Tau (andZero cx'))
            Yield _ cx' -> k (Tau (andZero cx'))
            Done cr -> k (Done cr)
        ))
    ||]
        where
            andZero cx = [|| ($$cx,0) ||]

takeWhileC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m ()
takeWhileC f (S cx0 next) = S [|| ($$cx0,True) ||] (\cx k -> [||
        let !(x,b) = $$cx in
        if not b then $$(k (Done [|| () ||])) else $$(next [||x||] (\case
            Effect cmx' -> k (Effect (fmapAction (with True) cmx'))
            Tau cx' -> k (Tau (with True cx'))
            Yield ca cx' -> [|| if $$(f ca) then $$(k (Yield ca (with True cx'))) else $$(k (Tau (with False cx'))) ||]
            Done _ -> k (Done [|| () ||])
        ))
    ||])
        where
            with b cx = [|| ($$cx,b) ||]

filterC :: (Code Q a -> Code Q Bool) -> Stream a m r -> Stream a m r
filterC p (S cx0 next) = S cx0 $ \cx k ->
    next cx (\case
        Effect cmx' -> k (Effect cmx')
        Tau cx' -> k (Tau cx')
        Yield ca cx' -> [||
            if $$(p ca) then $$(k (Yield ca cx')) else $$(k (Tau cx'))
         ||]
        Done cr -> k (Done cr)
    )

filter :: Code Q (a -> Bool) -> Stream a m r -> Stream a m r
filter f = filterC (\ca -> [|| $$f $$ca ||])

scanC :: (Code Q x -> Code Q a -> Code Q x) -> Code Q x -> (Code Q x -> Code Q b) -> Stream a m r -> Stream b m r
scanC step begin done (S cs0 next) = S [|| ($$cs0,$$begin) ||] (\csx k -> [||
        let !(s,x) = $$csx in
        $$(next [||s||] (\case
            Effect cms' -> k (Effect (fmapAction (\cs -> [|| ($$cs,x) ||]) cms') )
            Tau cms' -> k (Tau [|| ($$cms',x) ||])
            Yield ca cs' -> [||
                let !x' = $$(step [|| x ||] ca) in
                $$(k (Yield (done [|| x' ||]) [|| ($$cs',x') ||]))
             ||]
            Done cr -> k (Done cr)
        ))
    ||])

foldC :: Monad m => (Code Q x -> Code Q a -> Code Q x) -> Code Q x -> (Code Q x -> Code Q b) -> Stream a m r -> Code Q (m (b,r))
foldC step begin done (S cx0 next) = [|| do
    let loop !acc !x = $$(next [|| x ||] $ \case
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
fromListC cxs0 = S cxs0 $ \cxs k -> [||
    case $$cxs of
        [] -> $$(k (Done [|| () ||]))
        y:ys -> $$(k (Yield [|| y ||] [||ys||]))
  ||]

range :: (Lift a, Enum a, Ord a) => a -> a -> Stream a m ()
range lo hi = S [|| lo ||] $ \cn k -> [||
    if $$cn >= hi then $$(k (Done [|| () ||]))
    else $$(k (Yield [|| $$cn ||] [|| succ $$cn ||]))
 ||]



end :: Monad m => Stream a m r -> Code Q (m r)
end s = [|| (snd <$> $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s)) ||]

drain :: Monad m => Stream a m r -> Code Q (m ())
drain s = [|| (Control.Monad.void $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s))||]