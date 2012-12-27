module Bits

%default total

log2ceil : Nat -> Nat
log2ceil n = let x = log2 n in
             if x == log2 (n-1)
             then x+1
             else x

nextBits : Nat -> Type
nextBits x with (log2ceil x)
  | (S (S (S (S (S (S _)))))) = Bits64
  | (S (S (S (S (S O))))) = Bits32
  | (S (S (S (S O)))) = Bits16
  | _ = Bits8

abstract
data Bits : Nat -> Type where
    MkBits : {n : Nat} -> nextBits n -> Bits n

pad8 : Nat -> (Bits8 -> Bits8 -> Bits8) -> Bits8 -> Bits8 -> Bits8
pad8 n f x y = prim__lshrB8 (f (prim__shlB8 x pad) (prim__shlB8 y pad)) pad
    where
      pad = (prim__intToB8 (cast (8-n)))

pad16 : Nat -> (Bits16 -> Bits16 -> Bits16) -> Bits16 -> Bits16 -> Bits16
pad16 n f x y = prim__lshrB16 (f (prim__shlB16 x pad) (prim__shlB16 y pad)) pad
    where
      pad = (prim__intToB16 (cast (16-n)))

pad32 : Nat -> (Bits32 -> Bits32 -> Bits32) -> Bits32 -> Bits32 -> Bits32
pad32 n f x y = prim__lshrB32 (f (prim__shlB32 x pad) (prim__shlB32 y pad)) pad
    where
      pad = (prim__intToB32 (cast (32-n)))

pad64 : Nat -> (Bits64 -> Bits64 -> Bits64) -> Bits64 -> Bits64 -> Bits64
pad64 n f x y = prim__lshrB64 (f (prim__shlB64 x pad) (prim__shlB64 y pad)) pad
    where
      pad = (prim__intToB64 (cast (64-n)))

%assert_total
pow : Int -> Int -> Int
pow x y = if y > 0
          then x * (pow x (y - 1))
          else 1

%assert_total
intToBits' : {n: Nat} -> Int -> nextBits n
intToBits' {n=n} x with (nextBits n)
    | Bits8 = prim__intToB8 (x `mod` (pow 2 (cast n)))
    | Bits16 = prim__intToB16 (x `mod` (pow 2 (cast n)))
    | Bits32 = prim__intToB32 (x `mod` (pow 2 (cast n)))
    | Bits64 = prim__intToB64 (x `mod` (pow 2 (cast n)))

public
intToBits : {n : Nat} -> Int -> Bits n
intToBits n = MkBits (intToBits' n)

instance Cast Int (Bits n) where
    cast = intToBits

bitsShl' : {n: Nat} -> nextBits n -> nextBits n -> nextBits n
bitsShl' {n=n} x c with (nextBits n)
    | Bits8 = pad8 n prim__shlB8 x c
    | Bits16 = pad16 n prim__shlB16 x c
    | Bits32 = pad32 n prim__shlB32 x c
    | Bits64 = pad64 n prim__shlB64 x c

public
bitsShl : Bits n -> Bits n -> Bits n
bitsShl (MkBits x) (MkBits y) = MkBits (bitsShl' x y)

bitsLShr' : {n: Nat} -> nextBits n -> nextBits n -> nextBits n
bitsLShr' {n=n} x c with (nextBits n)
    | Bits8 = prim__lshrB8 x c
    | Bits16 = prim__lshrB16 x c
    | Bits32 = prim__lshrB32 x c
    | Bits64 = prim__lshrB64 x c

public
bitsLShr : Bits n -> Bits n -> Bits n
bitsLShr (MkBits x) (MkBits y) = MkBits (bitsLShr' x y)

bitsAShr' : {n: Nat} -> nextBits n -> nextBits n -> nextBits n
bitsAShr' {n=n} x c with (nextBits n)
    | Bits8 = prim__ashrB8 x c
    | Bits16 = prim__ashrB16 x c
    | Bits32 = prim__ashrB32 x c
    | Bits64 = prim__ashrB64 x c

public
bitsAShr : Bits n -> Bits n -> Bits n
bitsAShr (MkBits x) (MkBits y) = MkBits (bitsAShr' x y)

bitsAnd' : {n: Nat} -> nextBits n -> nextBits n -> nextBits n
bitsAnd' {n=n} x y with (nextBits n)
    | Bits8 = prim__andB8 x y
    | Bits16 = prim__andB16 x y
    | Bits32 = prim__andB32 x y
    | Bits64 = prim__andB64 x y

public
bitsAnd : Bits n -> Bits n -> Bits n
bitsAnd (MkBits x) (MkBits y) = MkBits (bitsAnd' x y)

bitsOr' : {n: Nat} -> nextBits n -> nextBits n -> nextBits n
bitsOr' {n=n} x y with (nextBits n)
    | Bits8 = prim__orB8 x y
    | Bits16 = prim__orB16 x y
    | Bits32 = prim__orB32 x y
    | Bits64 = prim__orB64 x y

public
bitsOr : Bits n -> Bits n -> Bits n
bitsOr (MkBits x) (MkBits y) = MkBits (bitsOr' x y)

bitsXOr' : {n: Nat} -> nextBits n -> nextBits n -> nextBits n
bitsXOr' {n=n} x y with (nextBits n)
    | Bits8 = prim__xorB8 x y
    | Bits16 = prim__xorB16 x y
    | Bits32 = prim__xorB32 x y
    | Bits64 = prim__xorB64 x y

public
bitsXOr : Bits n -> Bits n -> Bits n
bitsXOr (MkBits x) (MkBits y) = MkBits (bitsXOr' x y)

bitsAdd' : {n : Nat} -> nextBits n -> nextBits n -> nextBits n
bitsAdd' {n=n} x y with (nextBits n)
    | Bits8 = pad8 n prim__addB8 x y
    | Bits16 = pad16 n prim__addB16 x y
    | Bits32 = pad32 n prim__addB32 x y
    | Bits64 = pad64 n prim__addB64 x y

public
bitsAdd : Bits n -> Bits n -> Bits n
bitsAdd (MkBits x) (MkBits y) = MkBits (bitsAdd' x y)

bitsSub' : {n : Nat} -> nextBits n -> nextBits n -> nextBits n
bitsSub' {n=n} x y with (nextBits n)
    | Bits8 = pad8 n prim__subB8 x y
    | Bits16 = pad16 n prim__subB16 x y
    | Bits32 = pad32 n prim__subB32 x y
    | Bits64 = pad64 n prim__subB64 x y

public
bitsSub : Bits n -> Bits n -> Bits n
bitsSub (MkBits x) (MkBits y) = MkBits (bitsSub' x y)

bitsMul' : {n : Nat} -> nextBits n -> nextBits n -> nextBits n
bitsMul' {n=n} x y with (nextBits n)
    | Bits8 = pad8 n prim__mulB8 x y
    | Bits16 = pad16 n prim__mulB16 x y
    | Bits32 = pad32 n prim__mulB32 x y
    | Bits64 = pad64 n prim__mulB64 x y

public
bitsMul : Bits n -> Bits n -> Bits n
bitsMul (MkBits x) (MkBits y) = MkBits (bitsMul' x y)

bitsSDiv' : {n : Nat} -> nextBits n -> nextBits n -> nextBits n
bitsSDiv' {n=n} x y with (nextBits n)
    | Bits8 = prim__sdivB8 x y
    | Bits16 = prim__sdivB16 x y
    | Bits32 = prim__sdivB32 x y
    | Bits64 = prim__sdivB64 x y

public
bitsSDiv : Bits n -> Bits n -> Bits n
bitsSDiv (MkBits x) (MkBits y) = MkBits (bitsSDiv' x y)

bitsUDiv' : {n : Nat} -> nextBits n -> nextBits n -> nextBits n
bitsUDiv' {n=n} x y with (nextBits n)
    | Bits8 = prim__udivB8 x y
    | Bits16 = prim__udivB16 x y
    | Bits32 = prim__udivB32 x y
    | Bits64 = prim__udivB64 x y

public
bitsUDiv : Bits n -> Bits n -> Bits n
bitsUDiv (MkBits x) (MkBits y) = MkBits (bitsUDiv' x y)

bitsToStr' : {n : Nat} -> nextBits n -> String
bitsToStr' {n=n} x with (nextBits n)
    | Bits8 = prim__b8ToStr x
    | Bits16 = prim__b16ToStr x
    | Bits32 = prim__b32ToStr x
    | Bits64 = prim__b64ToStr x

public
bitsToStr : Bits n -> String
bitsToStr (MkBits x) = bitsToStr' x

bitsEq' : (x : nextBits n) -> (y : nextBits n) -> Int
bitsEq' {n=n} x y with (nextBits n)
    | Bits8 = prim__eqB8 x y
    | Bits16 = prim__eqB16 x y
    | Bits32 = prim__eqB32 x y
    | Bits64 = prim__eqB64 x y

public
bitsEq : (x : Bits n) -> (y : Bits n) -> Either (x = y) (x = y -> _|_)
bitsEq (MkBits x) (MkBits y) =
    case (bitsEq' x y) of
      0 => Right uneq
      _ => Left eq
    where
      postulate uneq : {x : Bits n} -> {y : Bits n} -> x = y -> _|_
      postulate eq : {x : Bits n} -> {y : Bits n} -> x = y
