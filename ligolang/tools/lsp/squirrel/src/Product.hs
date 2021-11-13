
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
  The heterogeneous list.
-}

module Product
  ( Product (..)
  , Contains (..)
  , (:=) (..)
  , putElem
  , getTag
  , modTag
  ) where

import GHC.Types

import Duplo.Pretty

-- | `Product xs` contains elements of each of the types from the `xs` list.
data Product xs where
  (:>) :: x -> Product xs -> Product (x : xs)
  Nil  :: Product '[]

infixr 5 :>

-- | Find/modify the element with a given type.
--
--   If you want to have same-types, use newtype wrappers.
--
class Contains x xs where
  getElem :: Product xs -> x
  modElem :: (x -> x) -> Product xs -> Product xs

instance {-# OVERLAPS #-} Contains x (x : xs) where
  getElem   (x :> _) = x
  modElem f (x :> xs) = f x :> xs

instance Contains x xs => Contains x (y : xs) where
  getElem   (_ :> xs) = getElem xs
  modElem f (x :> xs) = x :> modElem f xs

putElem :: Contains x xs => x -> Product xs -> Product xs
putElem = modElem . const

-- | Add a name to the type.
--
newtype (s :: Symbol) := t = Tag { unTag :: t }

-- | Retrieve a type associated with the given name.
--
getTag :: forall s t xs. Contains (s := t) xs => Product xs -> t
getTag = unTag . getElem @(s := t)

-- | Modify a type associated with the given name.
--
modTag
  :: forall s t xs
  .  Contains (s := t) xs
  => (t -> t)
  -> Product xs -> Product xs
modTag f = modElem @(s := t) (Tag . f . unTag)

instance Eq (Product '[]) where
  _ == _ = True

instance (Eq x, Eq (Product xs)) => Eq (Product (x : xs)) where
  x :> xs == y :> ys = x == y && xs == ys

class PrettyProd xs where
  ppProd :: Product xs -> Doc

instance {-# OVERLAPS #-} Pretty x => PrettyProd '[x] where
  ppProd (x :> Nil) = pp x

instance (Pretty x, PrettyProd xs) => PrettyProd (x : xs) where
  ppProd (x :> xs) = pp x <.> "," <+> ppProd xs

instance Pretty (Product '[]) where
  pp Nil = "{}"

instance PrettyProd xs => Pretty (Product xs) where
  pp = braces . ppProd

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pp (a, b) = pp a <.> ":" `indent` pp b
