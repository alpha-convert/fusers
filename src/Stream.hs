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
import Control.Monad

{-
We need to win on all of these benchmarks.
https://github.com/composewell/streaming-benchmarks/tree/master?tab=readme-ov-file
-}


{-
Having multi-steps turns things that would previously be complex into simple operations :)
-}
data Steps a m n r s where
    Effect :: Improve m n => n (Steps a m n r s) -> Steps a m n r s
    Yield :: CodeQ a -> Steps a m n r s -> Steps a m n r s
    Tau :: CodeQ s -> Steps a m n r s
    Done :: CodeQ r -> Steps a m n r s

{- This could be better, so we don't always generate binds before the recursive call to loop.
   In other words, if the steps value includes no effects, we should instead return a pure (x,Eiter s r).
-}
collectSteps :: Improve m n => Steps a m n r s -> n ([CodeQ a],Either (CodeQ s) (CodeQ r))
collectSteps (Effect nss) = nss >>= collectSteps
collectSteps (Yield a k) = do
    (as,end) <- collectSteps k
    return (a:as,end)
collectSteps (Tau cx) = return ([],Left cx)
collectSteps (Done cr) = return ([],Right cr)


stateMapC :: (CodeQ s -> CodeQ s') -> Steps a m n r s -> Steps a m n r s'
stateMapC f (Effect k) = Effect (stateMapC f <$> k)
stateMapC f (Yield a s) = Yield a (stateMapC f s)
stateMapC f (Tau x) = Tau (f x)
stateMapC _ (Done x) = Done x

data Stream a m r where
    S :: forall a m n r s. Improve m n => Gen (CodeQ s) -> (CodeQ s -> Gen (Steps a m n r s)) -> Stream a m r

{-
CONSTRUCTION
-}

range :: (Lift a, Enum a, Ord a) => a -> a -> Stream a m ()
range lo hi = S (return [|| lo ||]) $ \cn -> gen $ \k -> [||
    if $$cn >= hi then $$(k (Done [|| () ||]))
    else $$(k (Yield [|| $$cn ||] (Tau [|| succ $$cn ||])))
 ||]

repeat :: (Improve m n) => CodeQ (m a) -> Stream a m Void
repeat act = S (return [|| () ||]) $ \_ -> do
    return $ Effect $ do
        ca <- up act
        return (Yield ca (Tau [|| () ||]))

{-
COMBINATORS
-}

mapC :: (CodeQ a -> CodeQ b) -> Stream a m r -> Stream b m r
mapC f (S cx0 next) = S cx0 $ \cx -> do
    next cx >>= \case
        Effect ams -> return (Effect (_ <$> ams))
        Tau cx' -> return (Tau cx')
        Yield ca cx' -> return (Yield (f ca) (_ cx'))
        Done cr -> return (Done cr)

map :: CodeQ (a -> b) -> Stream a m r -> Stream b m r
map f = mapC (\ca -> [|| $$f $$ca ||])

mapMC :: (Improve m n) => (CodeQ a -> n (CodeQ b)) -> Stream a m r -> Stream b m r
mapMC f (S kcx0 next) = S (do {cx0 <- kcx0; return [|| ($$cx0,Nothing) ||]}) $ \cxm -> do
    (cx,cm) <- split cxm
    mc <- split cm
    case mc of
        Nothing -> next cx >>= \case
            Effect u -> return (Effect ((\cs -> [|| ($$cs,Nothing) ||]) <$> u))
            Tau cx' -> return (Tau [|| ($$cx',Nothing) ||])
            Yield ca cx' -> return (Effect $ do {cb <- f ca; return [|| ($$cx',Just $$cb) ||]})
            Done cr -> return (Done cr)
        Just ca -> return (Yield ca [|| ($$cx,Nothing) ||])


drop :: Int -> Stream a m r -> Stream a m r
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

dropWhileC :: (CodeQ a -> CodeQ Bool) -> Stream a m r -> Stream a m r
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

take :: Int -> Stream a m r -> Stream a m r
take n0 (S kcx0 next) = S (do {cx0 <- kcx0; return [|| ($$cx0,n0) ||]}) $ \cxn -> do
    (cx,cn) <- split cxn
    b <- split [|| $$cn > 0 ||]
    if not b then stateMapC andZero <$> next cx
    else do
        st <- next cx
        case st of
            Effect cmx' -> return (Effect ((\cs -> [|| ($$cs,$$cn) ||]) <$> cmx'))
            Tau cx' -> return (Tau [|| ($$cx', $$cn - 1) ||])
            Yield ca cx' -> return (Yield ca [|| ($$cx', $$cn - 1) ||])
            Done cr  -> return (Done cr)
        where
            andZero cx = [|| ($$cx,0) ||]

takeWhileC :: (CodeQ a -> CodeQ Bool) -> Stream a m r -> Stream a m ()
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

scanC :: (CodeQ x -> CodeQ a -> CodeQ x) -> CodeQ x -> (CodeQ x -> CodeQ b) -> Stream a m r -> Stream b m r
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

foldC :: forall a b x m r. Monad m => (CodeQ x -> CodeQ a -> CodeQ x) -> CodeQ x -> (CodeQ x -> CodeQ b) -> Stream a m r -> CodeQ (m (b,r))
foldC step begin done (S (kcx0 :: Gen (CodeQ s)) next) = unGen kcx0 $ \cx0 -> [|| do
    let loop (!acc,!x) = $$(runGen $ do {
        st <- next [||x||];
        let m = collectSteps st in
        _
        -- let u = collectSteps [||acc||] st
        -- return [|| $$(down $ collectSteps _ _) >>= _ ||]
    })
    -- \case
            -- Effect cmx' -> [|| $$(downAll [||acc||] [||x||] cmx') >>= loop ||]
            -- Tau cx' -> [|| loop (acc,$$cx') ||]
            -- Yield ca cx' -> [|| loop ($$(step [|| acc ||] ca),$$cx') ||]
            -- Done cr -> [|| return ($$(done [|| acc ||]),$$cr) ||]
        --  )
    loop ($$begin,$$cx0)
 ||]


toListC :: (Monad m) => Stream a m r -> CodeQ (m ([a],r))
toListC = foldC (\cdl ca -> [|| \ls -> $$cdl ($$ca : ls) ||]) [|| id ||] (\dl -> [|| $$dl [] ||])

sumC :: (Monad m) => Stream Int m r -> CodeQ (m Int)
sumC s = [|| $$(foldC (\cx cy -> [|| $$cx + $$cy ||]) [|| 0 ||] (\cx -> [|| return $$cx ||]) s) >>= fst ||]

fromList' :: [CodeQ a] -> Stream a m ()
fromList' xs = S (return [|| () ||]) $ \_ -> return (foldr Yield (Done [||()||]) xs)

fromListC :: CodeQ [a] -> Stream a m ()
fromListC cxs0 = S (return cxs0) $ \cxs -> gen $ \k -> [||
    case $$cxs of
        [] -> $$(k (Done [|| () ||]))
        y:ys -> $$(k (Yield [|| y ||] (Tau [||ys||])))
  ||]

end :: (Monad m) => Stream a m r -> CodeQ (m r)
end s = [|| (snd <$> $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s)) ||]

drain :: (Monad m) => Stream a m r -> CodeQ (m ())
drain s = [|| (Control.Monad.void $$(foldC (\_ _ -> [|| () ||]) [|| () ||] (const [|| () ||]) s))||]