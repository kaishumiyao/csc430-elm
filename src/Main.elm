module Main exposing (..)

import Html exposing (text)

type Value
    = NumV { num :  Float}
    | BoolV { b : Bool }
    | StrV  {str : String}
    | PrimV {f : List Value -> Value}
    | NullV  {msg : String}

numTest =  NumV { num = 30.0}

serialize : Value -> String

serialize val = 
    case val of 
        NumV n ->
            String.fromFloat n.num
        BoolV b ->
            "boolean"
        StrV s ->
            s.str
        NullV n ->
            "null"
        PrimV p ->
            "#procedure"



printF = 
    serialize numTest

main =
    text printF





