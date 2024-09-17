{-|
Module      : Data.Default.Singletons
Description : Provides singleton-based default values and optional types.
Copyright   : (c) 2024, Eitan Chatav
License     : MIT
Maintainer  : eitan.chatav@gmail.com
Stability   : experimental
Portability : non-portable (GHC extensions)

This module defines an `Opt`ional type with
either a `Def`ault promoted value,
or `Some` specific demoted value.

>>> definite (Def :: Opt True)
True
>>> definite (Some False :: Opt True)
False
>>> definite (Some True :: Opt True)
True
>>> maybe "def" show (perhaps (Def :: Opt True))
"def"
>>> maybe "def" show (perhaps (Some True :: Opt True))
"True"
>>> maybe "def" show (perhaps (Some False :: Opt True))
"False"

Demotable datakinds include:

>>> :kind! Demote Symbol
Demote Symbol :: *
= Text
>>> :kind! Demote Natural
Demote Natural :: *
= Natural
>>> :kind! Demote Char
Demote Char :: *
= Char
>>> :kind! Demote Bool
Demote Bool :: *
= Bool
>>> :kind! Demote [k]
Demote [k] :: *
= [Demote k]
>>> :kind! Demote String
Demote String :: *
= [Char]
>>> :kind! Demote (Either k j)
Demote (Either k j) :: *
= Either (Demote k) (Demote j)
>>> :kind! Demote (k,j)
Demote (k,j) :: *
= (Demote k, Demote j)

Because there is no promoted `Integer` and `Rational` datakinds,
this module defines them as `Z` and `Q`.

>>> :kind! Demote Z
Demote Z :: *
= Integer
>>> :kind! Demote Q
Demote Q :: *
= Ratio Integer

The `Opt` type comes with
`Num`, `Integral`, `Fractional`, and `Real` instances,
using `definite` values to do arithmetic;
and `IsString` and `IsList` instances,
which let you use literals to construct `Opt`.

>>> "text" :: Opt "hello"
Some "text"
>>> "string" :: Opt ['a','b','c']
Some "string"
>>> [1, 2] :: Opt ('[] :: [Natural])
Some [1,2]
>>> -5 :: Opt (Pos 0)
Some (-5)
>>> 3/4 :: Opt (Neg 7 :% 11 :: Q)
Some (3 % 4)
-}

{-# LANGUAGE
ConstraintKinds
, DataKinds
, FlexibleContexts
, FlexibleInstances
, GADTs
, LambdaCase
, PolyKinds
, RankNTypes
, ScopedTypeVariables
, StandaloneDeriving
, TypeApplications
, TypeFamilies
, TypeOperators
, UndecidableInstances
#-}

module Data.Default.Singletons
  ( -- | Optional Datatype
    Opt (..)
  , SingDef
  , optionally
  , definite
  , perhaps
    -- | Promoted Datakinds
  , Z (..)
  , Neg
  , Q (..)
  , SInteger (..)
  , SRational (..)
  ) where

import Control.Applicative
import Data.Default
import GHC.IsList
import Data.Ratio
import GHC.TypeNats
import Data.Singletons
import Data.String
import Prelude.Singletons ()

{- |
`Opt`ional type with either
a default value `Def`,
specified at the type level by @def@,
or a specific value `Some`.
-}
data Opt (def :: k) where
  Def :: SingDef def => Opt (def :: k)
  Some :: SingDef def => Demote k -> Opt (def :: k)

{- | Constraint required to `demote` @@def@. -}
type SingDef (def :: k) = (SingI def, SingKind k)

instance Semigroup (Opt (def :: k)) where
  Def <> opt = opt
  Some x <> _ = Some x

instance SingDef def => Monoid (Opt def) where
  mempty = Def

deriving instance (SingDef def, Show (Demote k))
  => Show (Opt (def :: k))

deriving instance (SingDef def, Read (Demote k))
  => Read (Opt (def :: k))

deriving instance (SingDef def, Eq (Demote k))
  => Eq (Opt (def :: k))

deriving instance (SingDef def, Ord (Demote k))
  => Ord (Opt (def :: k))

instance SingDef def
  => Default (Opt (def :: k)) where def = Def

instance (SingDef def, Num (Demote k))
  => Num (Opt (def :: k)) where
    x + y = Some $ definite x + definite y
    x * y = Some $ definite x * definite y
    abs x = Some $ abs (definite x)
    signum x = Some $ signum (definite x)
    fromInteger x = Some $ fromInteger x
    negate x = Some $ negate (definite x)
    x - y = Some $ definite x - definite y

instance (SingDef def, Fractional (Demote k))
  => Fractional (Opt (def :: k)) where
    recip x = Some $ recip (definite x)
    x / y = Some $ definite x / definite y
    fromRational x = Some $ fromRational x

instance (SingDef def, IsString (Demote k))
  => IsString (Opt (def :: k)) where
    fromString x = Some $ fromString x

instance (SingDef def, IsList (Demote k))
  => IsList (Opt (def :: k)) where
    type Item (Opt (def :: k)) = Item (Demote k)
    fromList xs = Some $ fromList xs
    fromListN n xs = Some $ fromListN n xs
    toList x = toList $ definite x

{- |
Constructs an `Opt` from a `Maybe`.
`Nothing` maps to `Def`,
and `Just` maps to `Some`.
-}
optionally :: SingDef def => Maybe (Demote k) -> Opt (def :: k)
optionally = maybe Def Some

{- |
Deconstructs an `Opt` to a `Demote`d value.
`Def` maps to `demote` @@def@,
and `Some` maps to its argument.
-}
definite :: forall k def. Opt (def :: k) -> Demote k
definite = \case
  Def -> demote @def
  Some a -> a

{- |
Deconstructs an `Opt` to an `Alternative` `Demote`d value.
`Def` maps to `empty`,
and `Some` maps to `pure`.
-}
perhaps :: Alternative m => Opt (def :: k) -> m (Demote k)
perhaps = \case
  Def -> empty
  Some a -> pure a

{- |
Datakind `Z`, promoting `Integer`,

>>> :kind! Demote Z
Demote Z :: *
= Integer

with `Pos` for constructing nonnegative integer types,
and `Neg` for constructing nonpositive integer types.

>>> demote @(Pos 90210)
90210
>>> demote @(Neg 5)
-5
>>> demote @(Neg 0)
0

-}
data Z = Pos Natural | NegOneMinus Natural
  deriving (Eq, Ord, Read, Show)

{- | Type family for negating a `Natural`.-}
type family Neg n where
  Neg 0 = Pos 0
  Neg n = NegOneMinus (n - 1)

instance Real Z where
  toRational = toRational . toInteger

instance Integral Z where
  toInteger (Pos n) = toInteger n
  toInteger (NegOneMinus n) = negate 1 - toInteger n
  quotRem x y =
    let (q,r) = quotRem (toInteger x) (toInteger y)
    in (fromInteger q, fromInteger r)
  divMod x y =
    let (q,r) = divMod (toInteger x) (toInteger y)
    in (fromInteger q, fromInteger r)

instance Enum Z where
  toEnum = fromIntegral
  fromEnum = fromIntegral

instance Num Z where
  x + y = fromInteger (toInteger x + toInteger y)
  x * y = fromInteger (toInteger x * toInteger y)
  abs x = fromInteger (abs (toInteger x))
  signum x = fromInteger (signum (toInteger x))
  negate x = fromInteger (negate (toInteger x))
  x - y = fromInteger (toInteger x - toInteger y)
  fromInteger x =
    if signum x >= 0
    then Pos (fromInteger x)
    else NegOneMinus (fromInteger (negate (1 + x)))

{- | Singleton representation for the `Z` kind. -}
data SInteger (n :: Z) where
  SPos :: SNat n -> SInteger (Pos n)
  SNegOneMinus :: SNat n -> SInteger (NegOneMinus n)

type instance Sing = SInteger

instance SingKind Z where
  type Demote Z = Integer
  fromSing = \case
    SPos n -> fromIntegral (fromSing n)
    SNegOneMinus n -> negate 1 - fromIntegral (fromSing n)
  toSing n = withSomeSing n SomeSing

instance KnownNat n => SingI (Pos n) where
  sing = SPos sing

instance KnownNat n => SingI (NegOneMinus n) where
  sing = SNegOneMinus sing

{- |
Datakind `Q`, promoting `Rational`,

>>> :kind! Demote Q
Demote Q :: *
= Ratio Integer

with `:%` for constructing rational types.

>>> demote @(Pos 7 :% 11)
7 % 11
>>> demote @(Neg 4 :% 2)
(-2) % 1

-}
data Q = (:%) Z Nat
  deriving (Eq, Ord, Show, Read)

instance Real Q where
  toRational (x :% y) = fromRational (toInteger x % toInteger y)

instance Fractional Q where
  recip (Pos x :% y) = (Pos y :% x)
  recip (NegOneMinus x :% y) = (NegOneMinus (y - 1) :% (1 + x))
  fromRational x = (fromInteger (numerator x) :% fromInteger (denominator x))

instance Num Q where
  x + y = fromRational (toRational x + toRational y)
  x * y = fromRational (toRational x * toRational y)
  abs x = fromRational (abs (toRational x))
  signum x = fromRational (signum (toRational x))
  negate x = fromRational (negate (toRational x))
  x - y = fromRational (toRational x - toRational y)
  fromInteger x = fromRational (fromInteger x)

{- | Singleton representation for the `Q` kind. -}
data SRational (n :: Q) where
  SRational :: SInteger n -> SNat m -> SRational (n :% m)

type instance Sing = SRational

instance SingKind Q where
  type Demote Q = Rational
  fromSing (SRational num denom)
    = fromRational
    $ toRational (fromSing num)
    / toRational (fromSing denom)
  toSing q = withSomeSing q SomeSing

instance (SingI num, SingI denom) => SingI (num :% denom) where
  sing = SRational sing sing
