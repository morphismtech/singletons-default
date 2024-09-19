{-|
Module      : Data.Default.Singletons
Description : Provides singleton-based default values and optional types.
Copyright   : (c) 2024, Eitan Chatav
License     : MIT
Maintainer  : eitan.chatav@gmail.com
Stability   : experimental
Portability : non-portable (GHC extensions)

This module defines an `Opt`ional type with
either a `Def`ault promoted value type,
or `Some` specific demoted value term.

>>> definite (Def :: Opt True)
True
>>> definite (Some False :: Opt (def :: Bool))
False
>>> definite (Some True :: Opt False)
True
>>> maybe "def" show (perhaps (Def :: Opt True))
"def"
>>> maybe "def" show (perhaps (Some True :: Opt True))
"True"
>>> maybe "def" show (perhaps (Some False :: Opt True))
"False"

The correspondence between promoted datakinds
and demoted datatypes is inexact.
Usually, `Demote` @t ~ t@, but not always such as:

>>> :kind! Demote Symbol
Demote Symbol :: *
= Text

Because there is no promoted `Integer` and `Rational` datakinds in base,
this module defines them as `Z` and `Q`.

>>> :kind! Demote Z
Demote Z :: *
= Integer
>>> :kind! Demote Q
Demote Q :: *
= Ratio Integer

The `Opt` type comes with
`Num`, `Integral`, `Fractional`, and `Real` instances,
using `definite` values to do arithmetic,
which let you use literals and arithmetic to construct
`Some` specific `Opt` value;

>>> 4 :: Opt 20
Some 4
>>> (Def + 0) * 1 :: Opt (Pos 8 :: Z)
Some 8
>>> 0.5 + Def :: Opt (Neg 1 % 3 :: Q)
Some (1 % 6)

and `IsString` and `IsList` instances.

>>> "text" :: Opt ("abc" :: Symbol)
Some "text"
>>> "string" :: Opt (['a','b','c'] :: String)
Some "string"
>>> [[1, 2],[3,4]] :: Opt ('[] :: [[Natural]])
Some [[1, 2],[3,4]]

`Opt` is a `Monoid` which yields
the leftmost specific value when there are `Some`,
and the `Def`ault value when there are none.

>>> definite (mempty :: Opt "xyz")
"xyz"
>>> definite (Def <> "abc" <> "qrs" <> Def :: Opt "xyz")
"abc"

You can use `Opt` as an optional function argument.

>>> :{
greet :: Opt "Anon" -> Text
greet name = "Welcome, " <> definite name <> "."
:}
>>> greet "Sarah"
"Welcome, Sarah."
>>> greet Def
"Welcome, Anon."

Or, you can use `Opt` as an optional field in your record type.

>>> :{
data Person = Person
  { name :: Text
  , age :: Natural
  , alive :: Opt (True :: Bool)
  }
:}
>>> let isAlive person = definite (alive person)
>>> let jim = Person {name = "Jim", age = 40, alive = Def}
>>> isAlive jim
True

You almost never need to include the datakind in your
type signatures since it's usually inferrable from @def@.

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
  , type (%)
  , type Reduce
  , type GCD
  , SInteger (..)
  , SRational (..)
    -- | Reexport Demote
  , demote
  , type Demote
  ) where

import Control.Applicative
import Data.Default
import GHC.IsList
import Data.Ratio
import GHC.TypeLits
import Data.Singletons
import Data.String
import Prelude.Singletons ()

{- |
`Opt`ional type with
either a `Def`ault promoted value @def@,
or `Some` specific `Demote`d value.
-}
data Opt (def :: k) where
  Def :: forall {k} def. SingDef def => Opt (def :: k)
  Some :: forall {k} def. Demote k -> Opt (def :: k)

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

instance Num (Demote k)
  => Num (Opt (def :: k)) where
    x + y = Some $ definite x + definite y
    x * y = Some $ definite x * definite y
    abs x = Some $ abs (definite x)
    signum x = Some $ signum (definite x)
    fromInteger x = Some $ fromInteger x
    negate x = Some $ negate (definite x)
    x - y = Some $ definite x - definite y

instance Fractional (Demote k)
  => Fractional (Opt (def :: k)) where
    recip x = Some $ recip (definite x)
    x / y = Some $ definite x / definite y
    fromRational x = Some $ fromRational x

instance IsString (Demote k)
  => IsString (Opt (def :: k)) where
    fromString x = Some $ fromString x

instance IsList (Demote k)
  => IsList (Opt (def :: k)) where
    type Item (Opt (def :: k)) = Item (Demote k)
    fromList xs = Some $ fromList xs
    fromListN n xs = Some $ fromListN n xs
    toList x = toList $ definite x

{- |
Constructs an `Opt` from a `Maybe`.
`Nothing` maps to `Def`,
and `Just` maps to `Some`.

>>> definite (optionally @'[ '[1,2],'[3]] Nothing)
[[1,2],[3]]
>>> definite (optionally @"foo" (Just "bar"))
"bar"
-}
optionally
  :: forall {k} def. SingDef def
  => Maybe (Demote k)
  -> Opt (def :: k)
optionally = maybe Def Some

{- |
Deconstructs an `Opt` to a `Demote`d value.
`Def` maps to `demote` @@def@,
and `Some` maps to its argument.
-}
definite :: forall {k} def. Opt (def :: k) -> Demote k
definite = \case
  Def -> demote @def
  Some a -> a

{- |
Deconstructs an `Opt` to an `Alternative` `Demote`d value.
`Def` maps to `empty`,
and `Some` maps to `pure`,
inverting `optionally`.
-}
perhaps
  :: forall {k} def m. Alternative m
  => Opt (def :: k) -> m (Demote k)
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
>>> demote @(Pos 0)
0

Non`Neg`ative integer types are matched cardinally by `Pos`,

>>> :kind! Pos 9
Pos 9 :: Z
= Pos 9
>>> :kind! Neg 0
Neg 0 :: Z
= Pos 0

and `Neg`ative integer types are matched ordinally by `NegOneMinus`.

>>> :kind! Neg 6
Neg 6 :: Z
= NegOneMinus 5
>>> :kind! Neg 1
Neg 1 :: Z
= NegOneMinus 0

-}
data Z = Pos Natural | NegOneMinus Natural
  deriving (Eq, Ord, Read, Show)

{- | `Neg`ate a `Natural` type . -}
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

with `:%` for constructing (unreduced) and matching rational types,

>>> demote @(Pos 7 :% 11)
7 % 11
>>> demote @(Neg 4 :% 6)
(-2) % 3
>>> :kind Pos 10 :% 10
Pos 10 :% 10 :: Q

and `%` and `Reduce` for constructing reduced rational types.

>>> :kind! Pos 14 % 49
Pos 14 % 49 :: Q
= Pos 2 :% 7
>>> type Percent n = Pos n :% 100
>>> :kind! Percent 10
Percent 10 :: Q
= Pos 10 :% 100
>>> :kind! Reduce (Percent 10)
Reduce (Percent 10) :: Q
= Pos 1 :% 10
-}
data Q = (:%) Z Natural
  deriving (Eq, Ord, Show, Read)

{- |
Perform reduction on a rational type, idempotently.

prop> Reduce (Reduce q) ~ Reduce q
-}
type family Reduce q :: Q where
  Reduce (z :% n) = z % n

{- |
Construct a rational type in reduced form.
-}
type family (%) z n :: Q where
  Pos 0 % 0 = Pos 0 :% 0
  Pos p % q = Pos (Div p (GCD p q)) :% Div q (GCD p q)
  NegOneMinus p % q
    = Neg (Div (1 + p) (GCD (1 + p) q))
    :% Div q (GCD (1 + p) q)

{- |
Construct the greatest common divisor of `Natural` types.
-}
type family GCD (a :: Natural) (b :: Natural) :: Natural where
  GCD 0 b = b
  GCD a 0 = a
  GCD a b = GCD b (Mod a b)

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
