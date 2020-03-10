module Main exposing (..)

import Html exposing (text)



type Value
    = NumV { num :  Float}
    | BoolV { b : Bool }
    | StrV  {str : String}
    | PrimV {f : List Value -> Value}
    | CloV { params : List String
            , body : ExprC
            , cloEnv : List Bind}
    | NullV  {msg : String}
    | Err

type alias Bind
    = { name : String,
      val : Value }

type Env
    = List Bind

type ExprC
    = NumC { num :  Float}
    | IfC { cond : ExprC
            , thn : ExprC 
            , els : ExprC}
    | IdC { sym : String}
    | StrC {str : String}
    | PrimC {f : List Value -> Value}
    | LamC { params : List String
            , body : ExprC}
    | AppC  { func : ExprC 
            , args : List ExprC}
    
topEnv = 
      [
       Bind  "true"  (BoolV {b = True})
        , Bind "false" (BoolV {b = False})
        , Bind  "+" (PrimV  {f = add})
        , Bind "-" (PrimV  {f = sub})
        , Bind  "*" (PrimV  {f = mul} )
        , Bind "/" (PrimV  {f = div} )
        , Bind "<=" (PrimV  {f = leq} )
        , Bind "equals?" (PrimV  {f = eqs} )] 

-- Primitive Functions 

add : List Value -> Value
add vals =
    case vals of
        [ NumV n1 , NumV n2 ] ->
             NumV {num = n1.num + n2.num}
        _ ->
             Err 
                 
sub : List Value -> Value
sub vals =
    case vals of
        [ NumV n1 , NumV n2 ] ->
             NumV {num = n1.num - n2.num}
        _ ->
             Err 
                 

mul : List Value -> Value
mul vals =
    case vals of
        [ NumV n1 , NumV n2 ] ->
             NumV {num = n1.num * n2.num}
        _ ->
             Err 
                 

div : List Value -> Value
div vals =
    case vals of
        [ NumV n1 , NumV n2 ] ->
             NumV {num = (n1.num / n2.num)}
        _ ->
             Err 
                 
leq: List Value -> Value
leq vals =
    case vals of
        [ NumV n1 , NumV n2 ] ->
             BoolV {b = (n1.num <= n2.num)}
        _ ->
             Err 

eqs: List Value -> Value
eqs vals =
    case vals of
        [ NumV n1 , NumV n2 ] ->
             BoolV {b = (n1.num == n2.num)}
        [ BoolV b1 , BoolV b2 ] ->
             BoolV {b = (b1.b == b2.b)}
        [ StrV s1, StrV s2] ->
             BoolV {b = (s1.str == s2.str)}
        _ ->
             Err 
                 
numTest =  NumC { num = 30.0}

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
        CloV c ->
            "#procedure"
        Err ->
            "DUNQ: ERROR"


interp : (ExprC) -> (List Bind) -> Value
interp exp en =
    case exp of
            NumC nC -> 
                NumV { num = nC.num}
            LamC l -> 
                CloV { params = l.params
                     , body = l.body
                     , cloEnv = en}
            StrC st ->
                StrV {str = st.str}
            PrimC p ->
                PrimV {f = p.f}

            --@TODO add IdC, IfC and AppC
            _ ->
                 Err

main =
    text (serialize (interp numTest topEnv))






