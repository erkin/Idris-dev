#ifndef _IDRISBITSTRING_H
#define _IDRISBITSTRING_H

VAL idris_b8CopyForGC(VM *vm, VAL a);
VAL idris_b16CopyForGC(VM *vm, VAL a);
VAL idris_b32CopyForGC(VM *vm, VAL a);
VAL idris_b64CopyForGC(VM *vm, VAL a);

VAL idris_b8(VM *vm, VAL a);
VAL idris_b16(VM *vm, VAL a);
VAL idris_b32(VM *vm, VAL a);
VAL idris_b64(VM *vm, VAL a);

VAL idris_b8Plus(VM *vm, VAL a, VAL b);
VAL idris_b8Minus(VM *vm, VAL a, VAL b);
VAL idris_b8Times(VM *vm, VAL a, VAL b);
VAL idris_b8UDiv(VM *vm, VAL a, VAL b);
VAL idris_b8SDiv(VM *vm, VAL a, VAL b);
VAL idris_b8Lt(VM *vm, VAL a, VAL b);
VAL idris_b8Gt(VM *vm, VAL a, VAL b);
VAL idris_b8Eq(VM *vm, VAL a, VAL b);
VAL idris_b8Lte(VM *vm, VAL a, VAL b);
VAL idris_b8Gte(VM *vm, VAL a, VAL b);
VAL idris_b8Not(VM *vm, VAL a);
VAL idris_b8And(VM *vm, VAL a, VAL b);
VAL idris_b8Or(VM *vm, VAL a, VAL b);
VAL idris_b8Neg(VM *vm, VAL a);
VAL idris_b8BAnd(VM *vm, VAL a, VAL b);
VAL idris_b8BOr(VM *vm, VAL a, VAL b);
VAL idris_b8XOr(VM *vm, VAL a, VAL b);
VAL idris_b8LSh(VM *vm, VAL a, VAL b);
VAL idris_b8RSh(VM *vm, VAL a, VAL b);

VAL idris_b16Eq(VM *vm, VAL a, VAL b);
VAL idris_b32Eq(VM *vm, VAL a, VAL b);
VAL idris_b64Eq(VM *vm, VAL a, VAL b);

VAL idris_b8Z16(VM *vm, VAL a);
VAL idris_b8Z32(VM *vm, VAL a);
VAL idris_b8Z64(VM *vm, VAL a);
VAL idris_b8S16(VM *vm, VAL a);
VAL idris_b8S32(VM *vm, VAL a);
VAL idris_b8S64(VM *vm, VAL a);

VAL idris_b16Plus(VM *vm, VAL a, VAL b);
VAL idris_b16Minus(VM *vm, VAL a, VAL b);
VAL idris_b16Times(VM *vm, VAL a, VAL b);
VAL idris_b16UDiv(VM *vm, VAL a, VAL b);
VAL idris_b16SDiv(VM *vm, VAL a, VAL b);
VAL idris_b16Lt(VM *vm, VAL a, VAL b);
VAL idris_b16Gt(VM *vm, VAL a, VAL b);
VAL idris_b16Eq(VM *vm, VAL a, VAL b);
VAL idris_b16Lte(VM *vm, VAL a, VAL b);
VAL idris_b16Gte(VM *vm, VAL a, VAL b);
VAL idris_b16Not(VM *vm, VAL a);
VAL idris_b16And(VM *vm, VAL a, VAL b);
VAL idris_b16Or(VM *vm, VAL a, VAL b);
VAL idris_b16Neg(VM *vm, VAL a);
VAL idris_b16BAnd(VM *vm, VAL a, VAL b);
VAL idris_b16BOr(VM *vm, VAL a, VAL b);
VAL idris_b16XOr(VM *vm, VAL a, VAL b);
VAL idris_b16LSh(VM *vm, VAL a, VAL b);
VAL idris_b16RSh(VM *vm, VAL a, VAL b);

VAL idris_b16Z32(VM *vm, VAL a);
VAL idris_b16Z64(VM *vm, VAL a);
VAL idris_b16S32(VM *vm, VAL a);
VAL idris_b16S64(VM *vm, VAL a);
VAL idris_b16T8(VM *vm, VAL a);

VAL idris_b32Plus(VM *vm, VAL a, VAL b);
VAL idris_b32Minus(VM *vm, VAL a, VAL b);
VAL idris_b32Times(VM *vm, VAL a, VAL b);
VAL idris_b32UDiv(VM *vm, VAL a, VAL b);
VAL idris_b32SDiv(VM *vm, VAL a, VAL b);
VAL idris_b32Lt(VM *vm, VAL a, VAL b);
VAL idris_b32Gt(VM *vm, VAL a, VAL b);
VAL idris_b32Eq(VM *vm, VAL a, VAL b);
VAL idris_b32Lte(VM *vm, VAL a, VAL b);
VAL idris_b32Gte(VM *vm, VAL a, VAL b);
VAL idris_b32Not(VM *vm, VAL a);
VAL idris_b32And(VM *vm, VAL a, VAL b);
VAL idris_b32Or(VM *vm, VAL a, VAL b);
VAL idris_b32Neg(VM *vm, VAL a);
VAL idris_b32BAnd(VM *vm, VAL a, VAL b);
VAL idris_b32BOr(VM *vm, VAL a, VAL b);
VAL idris_b32XOr(VM *vm, VAL a, VAL b);
VAL idris_b32LSh(VM *vm, VAL a, VAL b);
VAL idris_b32RSh(VM *vm, VAL a, VAL b);

VAL idris_b32Z64(VM *vm, VAL a);
VAL idris_b32S64(VM *vm, VAL a);
VAL idris_b32T8(VM *vm, VAL a);
VAL idris_b32T16(VM *vm, VAL a);

VAL idris_b64Plus(VM *vm, VAL a, VAL b);
VAL idris_b64Minus(VM *vm, VAL a, VAL b);
VAL idris_b64Times(VM *vm, VAL a, VAL b);
VAL idris_b64UDiv(VM *vm, VAL a, VAL b);
VAL idris_b64SDiv(VM *vm, VAL a, VAL b);
VAL idris_b64Lt(VM *vm, VAL a, VAL b);
VAL idris_b64Gt(VM *vm, VAL a, VAL b);
VAL idris_b64Eq(VM *vm, VAL a, VAL b);
VAL idris_b64Lte(VM *vm, VAL a, VAL b);
VAL idris_b64Gte(VM *vm, VAL a, VAL b);
VAL idris_b64Not(VM *vm, VAL a);
VAL idris_b64And(VM *vm, VAL a, VAL b);
VAL idris_b64Or(VM *vm, VAL a, VAL b);
VAL idris_b64Neg(VM *vm, VAL a);
VAL idris_b64BAnd(VM *vm, VAL a, VAL b);
VAL idris_b64BOr(VM *vm, VAL a, VAL b);
VAL idris_b64XOr(VM *vm, VAL a, VAL b);
VAL idris_b64LSh(VM *vm, VAL a, VAL b);
VAL idris_b64RSh(VM *vm, VAL a, VAL b);

VAL idris_b64T8(VM *vm, VAL a);
VAL idris_b64T16(VM *vm, VAL a);
VAL idris_b64T32(VM *vm, VAL a);

#endif
