{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Split where
import Language.Haskell.TH
import Gen

{-
Stolen entirely from:
https://github.com/AndrasKovacs/staged/blob/main/icfp24paper/supplement/haskell-cftt/CFTT/Split.hs

This is "the trick" from https://dl.acm.org/doi/10.1145/236114.236119
-}

class Split a b | a -> b, b -> a where
  splitGen :: CodeQ a -> Gen b

split :: MonadGen m => Split a b => CodeQ a -> m b
split a = liftGen (splitGen a)

caseM :: (MonadGen m) => Split a b => CodeQ a -> (b -> m c) -> m c
caseM a f = split a >>= f

instance Split Bool Bool where
  splitGen x = Gen $ \k -> [|| (if $$x then $$(k True) else $$(k False)) ||]

instance Split [a] (Maybe (CodeQ a, CodeQ [a])) where
  splitGen x = Gen $ \k -> [|| case $$x of
    []   -> $$(k Nothing)
    a:as -> $$(k (Just ([||a||], [||as||]))) ||]

instance Split (a, b) (CodeQ a, CodeQ b) where
  splitGen x = Gen $ \k -> [|| case $$x of
    (a, b) -> $$(k ([||a||], [||b||])) ||]

instance Split (Either a b) (Either (CodeQ a) (CodeQ b)) where
  splitGen x = Gen $ \k -> [|| case $$x of
    Left a  -> $$(k (Left [||a||]))
    Right b -> $$(k (Right [||b||])) ||]

instance Split (Maybe a) (Maybe (CodeQ a)) where
  splitGen x = Gen $ \k -> [|| case $$x of
    Nothing -> $$(k Nothing)
    Just a  -> $$(k (Just [||a||])) ||]