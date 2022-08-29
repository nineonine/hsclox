module Value where

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
  , values   :: Maybe Value
}

initValueArray :: ValueArray
initValueArray = ValueArray 0 0 Nothing
