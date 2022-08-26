module Value where

import Object

data ValueType
    = VAL_BOOL
    | VAL_NIL
    | VAL_NUMBER
    | VAL_OBJ

data Value = Value {
    valueType :: ValueType
  , as :: ValueU
}

data ValueU
    = ValBool Bool
    | ValDbl Double
    | Obj

data ValueArray = ValueArray {
    capacity :: Int
  , count    :: Int
  , values   :: Value
}
