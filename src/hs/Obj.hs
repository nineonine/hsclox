module Obj where

import Data.ByteString.Lazy

data ObjFunction = ObjFunction {
    obj :: Obj
  , arity :: Int
  , upvalueCount :: Int
  , chunk :: Chunk
  , name  :: ObjString
}

data ObjString = ObjString {
    obj :: Obj
  , length :: Int
  , hash :: Int
  , chars :: ByteString
}

data Obj = Obj {
    objType :: ObjType
  , isMarked :: Bool
  , next :: Obj
}

data ObjType
    = OBJ_BOUND_METHOD
    | OBJ_CLASS
    | OBJ_CLOSURE
    | OBJ_FUNCTION
    | OBJ_INSTANCE
    | OBJ_NATIVE
    | OBJ_STRING
    | OBJ_UPVALUE
