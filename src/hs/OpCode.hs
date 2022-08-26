module OpCode where

import Data.Word

data OpCode
    = OP_CONSTANT
    | OP_NIL
    | OP_TRUE
    | OP_FALSE
    | OP_POP
    | OP_POPN
    | OP_SET_LOCAL
    | OP_GET_LOCAL
    | OP_GET_GLOBAL
    | OP_GET_UPVALUE
    | OP_SET_UPVALUE
    | OP_DEFINE_GLOBAL
    | OP_SET_GLOBAL
    | OP_GET_PROPERTY
    | OP_SET_PROPERTY
    | OP_GET_EXPR_PROPERTY
    | OP_SET_EXPR_PROPERTY
    | OP_GET_SUPER
    | OP_EQUAL
    | OP_GREATER
    | OP_LESS
    | OP_CONSTANT_LONG
    | OP_ADD
    | OP_SUBTRACT
    | OP_MULTIPLY
    | OP_DIVIDE
    | OP_NOT
    | OP_NEGATE
    | OP_PRINT
    | OP_JUMP
    | OP_JUMP_IF_FALSE
    | OP_JUMP_BREAK
    | OP_LOOP
    | OP_STACK_DUP_1
    | OP_CALL
    | OP_INVOKE
    | OP_SUPER_INVOKE
    | OP_CLOSURE
    | OP_CLOSE_UPVALUE
    | OP_RETURN
    | OP_CLASS
    | OP_INHERIT
    | OP_METHOD
    deriving (Eq, Show, Enum)

toBytes :: OpCode -> Word8
toBytes = fromIntegral . fromEnum
