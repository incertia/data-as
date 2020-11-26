{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module: Data.As
Description: Simple extensible sum
Copyright: (c) incertia, 2020
License: MIT
Maintainer: incertia@incertia.net
Stability: experimental
Portability: portable

This module provides the 'As' class which is a multi parameter classy prism,
much like how 'Has' is a multi parameter classy lens.

We have the following primary use case for 'As'.

@
 -- some library code
 throwE :: (As e err, MonadError err m) => e -> m ()
 throwE = throwError . review asPrism
@
-}

module Data.As
  ( As(..)
  ) where

import Data.Functor.Const
  (Const(..))
import Data.Functor.Identity
  (Identity(..))
import Data.Monoid
  (First(..))
import Data.Profunctor
  (Profunctor, Choice(..), dimap, right')
import Data.Profunctor.Unsafe
  ((.#), (#.))
import Data.Void
  (Void, absurd)
import Text.Read
  (readMaybe)

-- inline our own Tagged
newtype Tagged s b = Tagged { unTagged :: b }

instance Profunctor Tagged where
  dimap _ f (Tagged s) = Tagged (f s)

instance Choice Tagged where
  left' (Tagged b) = Tagged (Left b)
  right' (Tagged b) = Tagged (Right b)

-- inline local Prism type so we don't depend on lens
-- unfortunately this means we need to depend on profunctor
type Prism t a = forall p f. (Choice p, Applicative f) => p a (f a) -> p t (f t)

-- these definitions are ripped directly from lens
class As a t where
  {-# MINIMAL previewer, reviewer | asPrism #-}
  previewer :: t -> Maybe a
  previewer = getFirst #. getConst #. asPrism (Const #. First #. Just)

  reviewer :: a -> t
  reviewer = runIdentity #. unTagged #. asPrism .# Tagged .# Identity

  asPrism :: Prism t a
  asPrism = dimap (\a -> maybe (Left a) Right (previewer a)) (either pure (fmap reviewer)) . right'

  modifier :: (a -> a) -> t -> t
  modifier f = runIdentity . asPrism (Identity . f)

-- some instances based on the lens library
instance As a a where
  asPrism = id

instance As a (Maybe a) where
  previewer = id
  reviewer = Just

instance As () (Maybe a) where
  previewer ma = case ma of
                   Nothing -> Just ()
                   Just _  -> Nothing
  reviewer () = Nothing

instance As a (Either a b) where
  previewer eab = case eab of
                    Left a  -> Just a
                    Right _ -> Nothing
  reviewer = Left

instance As b (Either a b) where
  previewer eab = case eab of
                    Right b -> Just b
                    Left _  -> Nothing
  reviewer = Right

instance (Read a, Show a) => As a String where
  previewer = readMaybe
  reviewer = show

instance As Void a where
  previewer _ = Nothing
  reviewer = absurd
