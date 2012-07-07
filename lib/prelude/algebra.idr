module prelude.algebra

import builtins

-- XXX: change?
infixl 8 +,-
infixl 9 *,/


%access public

--------------------------------------------------------------------------------
-- A modest class hierarchy
--------------------------------------------------------------------------------

-- Sets equipped with a single binary operation that is associative.  Must
-- satisfy the following laws:
--   Associativity of +:
--     forall a b c, a + (b + c) == (a + b) + c
class Semigroup a where
  (+) : a -> a -> a

class Semigroup a => VerifiedSemigroup a where
  semigroupOpIsAssociative : (l, c, r : a) -> l + (c + r) = (l + c) + r

-- Sets equipped with a single binary operation that is associative, along with
-- a neutral element for that binary operation.  Must satisfy the following
-- laws:
--   Associativity of +:
--     forall a b c, a + (b + c) == (a + b) + c
--   Neutral for +:
--     forall a,     a + neutral   == a
--     forall a,     neutral + a   == a
class Semigroup a => Monoid a where
  fromInteger : Int -> a
  neutral : a

class (VerifiedSemigroup a, Monoid a) => VerifiedMonoid a where
  monoidNeutralIsNeutralL : (l : a) -> l + neutral = l
  monoidNeutralIsNeutralR : (r : a) -> neutral + r = r

-- Sets equipped with a single binary operation that is associative, along with
-- a neutral element for that binary operation and inverses for all elements.
-- Must satisfy the following laws:
--   Associativity of +:
--     forall a b c, a + (b + c) == (a + b) + c
--   Neutral for +:
--     forall a,     a + neutral   == a
--     forall a,     neutral + a   == a
--   Inverse for +:
--     forall a,     a + inverse a == neutral
--     forall a,     inverse a + a == neutral
class Monoid a => Group a where
  inverse : a -> a
  (-) : a -> a -> a

  inverse x = neutral - x
  (-) x y = x + (inverse y)

class (VerifiedMonoid a, Group a) => VerifiedGroup a where
  groupInverseIsInverseL : (l : a) -> l + inverse l = neutral
  groupInverseIsInverseR : (r : a) -> inverse r + r = neutral

-- Sets equipped with a single binary operation that is associative and
-- commutative, along with a neutral element for that binary operation and
-- inverses for all elements. Must satisfy the following laws:
--   Associativity of +:
--     forall a b c, a + (b + c) == (a + b) + c
--   Commutativity of +:
--     forall a b,   a + b         == b + a
--   Neutral for +:p
--     forall a,     a + neutral   == a
--     forall a,     neutral + a   == a
--   Inverse for +:
--     forall a,     a + inverse a == neutral
--     forall a,     inverse a + a == neutral
class Group a => AbelianGroup a where { }

class (VerifiedGroup a, AbelianGroup a) => VerifiedAbelianGroup a where
  abelianGroupOpIsCommutative : (l, r : a) -> l + r = r + l

-- Sets equipped with two binary operations, one associative and commutative
-- supplied with a neutral element, and the other associative, with
-- distributivity laws relating the two operations.  Must satisfy the following
-- laws:
--   Associativity of +:
--     forall a b c, a + (b + c) == (a + b) + c
--   Commutativity of +:
--     forall a b,   a + b         == b + a
--   Neutral for +:
--     forall a,     a + neutral   == a
--     forall a,     neutral + a   == a
--   Inverse for +:
--     forall a,     a + inverse a == neutral
--     forall a,     inverse a + a == neutral
--   Associativity of *:
--     forall a b c, a * (b * c) == (a * b) * c
--   Distributivity of * and -:
--     forall a b c, a * (b + c) == (a * b) + (a * c)
--     forall a b c, (a + b) * c == (a * c) + (b * c)
class AbelianGroup a => Ring a where
  (*) : a -> a -> a

class (VerifiedAbelianGroup a, Ring a) => VerifiedRing a where
  ringOpIsAssociative   : (l, c, r : a) -> l * (c * r) = (l * c) * r
  ringOpIsDistributiveL : (l, c, r : a) -> l * (c + r) = (l * c) + (l * r)
  ringOpIsDistributiveR : (l, c, r : a) -> (l + c) * r = (l * r) + (l * c)

-- Sets equipped with two binary operations, one associative and commutative
-- supplied with a neutral element, and the other associative supplied with a
-- neutral element, with distributivity laws relating the two operations.  Must
-- satisfy the following laws:
--   Associativity of +:
--     forall a b c, a + (b + c) == (a + b) + c
--   Commutativity of +:
--     forall a b,   a + b         == b + a
--   Neutral for +:
--     forall a,     a + neutral   == a
--     forall a,     neutral + a   == a
--   Inverse for +:
--     forall a,     a + inverse a == neutral
--     forall a,     inverse a + a == neutral
--   Associativity of *:
--     forall a b c, a * (b * c) == (a * b) * c
--   Neutral for *:
--     forall a,     a * unity     == a
--     forall a,     unity * a     == a
--   Distributivity of * and -:
--     forall a b c, a * (b + c) == (a * b) + (a * c)
--     forall a b c, (a + b) * c == (a * c) + (b * c)
class Ring a => RingWithUnity a where
  unity : a

class (VerifiedRing a, RingWithUnity a) => VerifiedRingWithUnity a where
  ringWithUnityIsUnityL : (l : a) -> l * unity = l
  ringWithUnityIsUnityR : (r : a) -> unity * r = r

-- TODO: docs
class RingWithUnity a => DivisionRing a where
  (/) : a -> a -> a

-- FIXME: This doesn't parse
-- class (VerifiedRingWithUnity a, DivisionRing a) => VerifiedDivisionRing a where
--   multiplicativeInversesExistL : (l : a) -> ((r : a) ** (l * r = 1))
--   multiplicativeInversesExistR : (r : a) -> ((l : a) ** (l * r = 1))

-- FIXME: Dependency cycle: Either uses List uses us
-- class VerifiedRing a => VerifiedDomain a where
--   zeroProductProperty : (l, r : a) -> l * r = 0 -> Either (l = 0) (r = 0)

class VerifiedRing a => VerifiedCommutativeRing a where
  multiplicationIsCommutative : (l, r : a) -> l * r = r * l

-- class (VerifiedDomain a, VerifiedCommutativeRing a) => VerifiedIntegralDomain a where
--   integralDomainIsNontrivial : (l : a) -> (r : a) ** (l = r) -> _|_

-- TODO: mod, associate/unit
class Ring a => EuclidianDomain a where
  div : a -> a -> a
  rem : a -> a -> a
  
  rem a b = a - b * (a `div` b)

class (EuclidianDomain a, DivisionRing a) => Field a where {}

-- Sets equipped with a binary operation that is commutative, associative and
-- idempotent.  Must satisfy the following laws:
--   Associativity of join:
--     forall a b c, join a (join b c) == join (join a b) c
--   Commutativity of join:
--     forall a b,   join a b          == join b a
--   Idempotency of join:
--     forall a,     join a a          == a
--  Join semilattices capture the notion of sets with a "least upper bound".
class JoinSemilattice a where
  join : a -> a -> a

class JoinSemilattice a => VerifiedJoinSemilattice a where
  joinSemilatticeJoinIsAssociative : (l, c, r : a) -> join l (join c r) = join (join l c) r
  joinSemilatticeJoinIsCommutative : (l, r : a)    -> join l r = join r l
  joinSemilatticeJoinIsIdempotent  : (e : a)       -> join e e = e

-- Sets equipped with a binary operation that is commutative, associative and
-- idempotent.  Must satisfy the following laws:
--   Associativity of meet:
--     forall a b c, meet a (meet b c) == meet (meet a b) c
--   Commutativity of meet:
--     forall a b,   meet a b          == meet b a
--   Idempotency of meet:
--     forall a,     meet a a          == a
--  Meet semilattices capture the notion of sets with a "greatest lower bound".
class MeetSemilattice a where
  meet : a -> a -> a

class MeetSemilattice a => VerifiedMeetSemilattice a where
  meetSemilatticeMeetIsAssociative : (l, c, r : a) -> meet l (meet c r) = meet (meet l c) r
  meetSemilatticeMeetIsCommutative : (l, r : a)    -> meet l r = meet r l
  meetSemilatticeMeetIsIdempotent  : (e : a)       -> meet e e = e

-- Sets equipped with a binary operation that is commutative, associative and
-- idempotent and supplied with a neutral element.  Must satisfy the following
-- laws:
--   Associativity of join:
--     forall a b c, join a (join b c) == join (join a b) c
--   Commutativity of join:
--     forall a b,   join a b          == join b a
--   Idempotency of join:
--     forall a,     join a a          == a
--   Bottom:
--     forall a,     join a bottom     == bottom
--  Join semilattices capture the notion of sets with a "least upper bound"
--  equipped with a "bottom" element.
class JoinSemilattice a => BoundedJoinSemilattice a where
  bottom  : a

class (VerifiedJoinSemilattice a, BoundedJoinSemilattice a) => VerifiedBoundedJoinSemilattice a where
  boundedJoinSemilatticeBottomIsBottom : (e : a) -> join e bottom = bottom

-- Sets equipped with a binary operation that is commutative, associative and
-- idempotent and supplied with a neutral element.  Must satisfy the following
-- laws:
--   Associativity of meet:
--     forall a b c, meet a (meet b c) == meet (meet a b) c
--   Commutativity of meet:
--     forall a b,   meet a b          == meet b a
--   Idempotency of meet:
--     forall a,     meet a a          == a
--   Top:
--     forall a,     meet a top        == top
--  Meet semilattices capture the notion of sets with a "greatest lower bound"
--  equipped with a "top" element.
class MeetSemilattice a => BoundedMeetSemilattice a where
  top : a

class (VerifiedMeetSemilattice a, BoundedMeetSemilattice a) => VerifiedBoundedMeetSemilattice a where
  boundedMeetSemilatticeTopIsTop : (e : a) -> meet e top = top

-- Sets equipped with two binary operations that are both commutative,
-- associative and idempotent, along with absorbtion laws for relating the two
-- binary operations.  Must satisfy the following:
--   Associativity of meet and join:
--     forall a b c, meet a (meet b c) == meet (meet a b) c
--     forall a b c, join a (join b c) == join (join a b) c
--   Commutativity of meet and join:
--     forall a b,   meet a b          == meet b a
--     forall a b,   join a b          == join b a
--   Idempotency of meet and join:
--     forall a,     meet a a          == a
--     forall a,     join a a          == a
--   Absorbtion laws for meet and join:
--     forall a b,   meet a (join a b) == a
--     forall a b,   join a (meet a b) == a
class (JoinSemilattice a, MeetSemilattice a) => Lattice a where { }

class (VerifiedJoinSemilattice a, VerifiedMeetSemilattice a) => VerifiedLattice a where
  latticeMeetAbsorbsJoin : (l, r : a) -> meet l (join l r) = l
  latticeJoinAbsorbsMeet : (l, r : a) -> join l (meet l r) = l

-- Sets equipped with two binary operations that are both commutative,
-- associative and idempotent and supplied with neutral elements, along with
-- absorbtion laws for relating the two binary operations.  Must satisfy the
-- following:
--   Associativity of meet and join:
--     forall a b c, meet a (meet b c) == meet (meet a b) c
--     forall a b c, join a (join b c) == join (join a b) c
--   Commutativity of meet and join:
--     forall a b,   meet a b          == meet b a
--     forall a b,   join a b          == join b a
--   Idempotency of meet and join:
--     forall a,     meet a a          == a
--     forall a,     join a a          == a
--   Absorbtion laws for meet and join:
--     forall a b,   meet a (join a b) == a
--     forall a b,   join a (meet a b) == a
--   Neutral for meet and join:
--     forall a,     meet a top        == top
--     forall a,     join a bottom     == bottom
class (BoundedJoinSemilattice a, BoundedMeetSemilattice a) => BoundedLattice a where { }

class (VerifiedBoundedJoinSemilattice a, VerifiedBoundedMeetSemilattice a, VerifiedLattice a) => VerifiedBoundedLattice a where { }
  
  
-- XXX todo:
--   Fields and vector spaces.
--   Where to put fromInteger and fromRational?


-- Algebras on primitive number types
-- Int instances
instance Semigroup Int where
  (+) = prim__addInt

instance Monoid Int where
  fromInteger = id
  neutral = 0

instance Group Int where
  (-) = prim__subInt

instance AbelianGroup Int where {}

instance Ring Int where
  (*) = prim__mulInt

instance RingWithUnity Int where
  unity = 1

instance EuclidianDomain Int where
  div = prim__divInt

-- Integer instances
instance Semigroup Integer where
  (+) = prim__addBigInt

instance Monoid Integer where
  fromInteger = prim__intToBigInt
  neutral = 0

instance Group Integer where
  (-) = prim__subBigInt

instance AbelianGroup Integer where {}

instance Ring Integer where
  (*) = prim__mulBigInt

instance RingWithUnity Integer where
  unity = 1

-- Float instances
instance Semigroup Float where
  (+) = prim__addFloat

instance Monoid Float where
  fromInteger = prim__intToFloat
  neutral = 0

instance Group Float where
  (-) = prim__subFloat

instance AbelianGroup Float where {}

instance Ring Float where
  (*) = prim__mulFloat

instance RingWithUnity Float where
  unity = 1

instance EuclidianDomain Float where
  div = prim__divFloat
  rem _ _ = 0

instance DivisionRing Float where
  (/) = prim__divFloat

instance Field Float where {}
