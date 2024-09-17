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
  ( Opt (..)
  , SingDef
  , optionally
  , definite
  , perhaps
  , Z (..)
  , Neg
  , Q (..)
  , SRational (..)
  , SInteger (..)
  ) where

import Control.Applicative
import Data.Default
import GHC.IsList
import Data.Ratio
import GHC.TypeNats
import Data.Singletons
import Data.String
import Prelude.Singletons ()

data Opt (def :: k) where
  Def
    :: (SingDef def)
    => Opt (def :: k)
  Some
    :: (SingDef def)
    => Demote k
    -> Opt (def :: k)

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

optionally :: SingDef def => Maybe (Demote k) -> Opt (def :: k)
optionally = maybe Def Some

definite :: forall k def. Opt (def :: k) -> Demote k
definite = \case
  Def -> demote @def
  Some a -> a

perhaps :: Alternative m => Opt (def :: k) -> m (Demote k)
perhaps = \case
  Def -> empty
  Some a -> pure a

data Z = Pos Natural | NegOneMinus Natural
  deriving (Eq, Ord, Read, Show)

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
