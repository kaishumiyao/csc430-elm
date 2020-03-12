module Main exposing (..)

import Html exposing (text)
import String exposing (toFloat, slice, dropLeft, dropRight, split)
import List exposing (head, tail, length)
import Array exposing (toList, fromList, get)

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
            NumV {num = n1.num / n2.num}
        _ ->
            Err 
                
leq: List Value -> Value
leq vals =
    case vals of
        [ NumV n1 , NumV n2 ] ->
            BoolV {b = n1.num <= n2.num}
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
            IdC i ->
                envlookup i.sym en
            IfC iC ->
                let 
                    testval = interp iC.cond en
                in
                case testval of
                    BoolV b ->
                        if b.b then
                            interp iC.thn en
                        else
                            interp iC.els en
                    _ ->
                        Err
            AppC ap ->
                let
                    fvalue = interp ap.func en
                in
                case fvalue of
                    CloV c ->
                        interp c.body (envhelper c.params ap.args c.cloEnv en)
                    PrimV p ->
                        p.f (List.map (\arg -> interp arg en) ap.args)
                    _ ->
                        Err


envhelper : List String -> List ExprC -> List Bind -> List Bind -> List Bind
envhelper params args cenv oenv =
  if List.isEmpty params && List.isEmpty args then
      cenv
  else if not (List.isEmpty params) && not (List.isEmpty args) then
      case params of
          [] ->
              Debug.todo "Wont come here"
          p :: px ->
              case args of
                  [] ->
                      Debug.todo "Wont come here"
                  a :: ax ->
                      envhelper px ax (Bind p (interp a oenv) :: cenv) oenv
  else
      [] --not sure how to catch error here


envlookup : String -> List Bind -> Value
envlookup n en = 
    case en of
        [] -> 
            Err
        x :: xs ->
            if x.name == n then
                x.val
            else
              envlookup n xs


listIndex : Int -> List a -> a
listIndex index list =
    if  (List.length list) >= index then
        let
            head = List.take index list
                    |> List.reverse
                    |> List.head
        in
            case head of
                Just val ->
                    val
                Nothing ->
                    Debug.todo "error"
    else
        Debug.todo "error"


parse : List String -> ExprC
parse exp =
    if length exp == 1 then
        let
            firstMaybe = head exp
        in
            case firstMaybe of
                Just first ->
                    let
                        f = String.toFloat first
                    in
                        case f of
                            Just num ->
                                NumC {num = num}
                            Nothing ->
                                if slice 0 1 first == "'" then
                                    IdC {sym = dropLeft 1 first}
                                else
                                    StrC {str = first}
                Nothing ->
                    Debug.todo "error"

    else if length exp > 1 then
        let
            firstMaybe = head exp
        in
            case firstMaybe of
                Just first ->
                    if first == "if" then
                        let
                            f1 = split " " (listIndex 2 exp)
                            f2 = split " " (listIndex 3 exp)
                            f3 = split " " (listIndex 4 exp)
                        in
                            IfC {cond = parse f1,
                                thn = parse f2,
                                els = parse f3}
                    else
                        Debug.todo "error"
                Nothing ->
                    Debug.todo "error"
    else
        Debug.todo "error"


main =
    text (serialize (interp numTest topEnv))






