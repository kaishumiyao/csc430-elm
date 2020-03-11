module Example exposing (..)

import Main as Main
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
    test "two plus two equals four"
        (\_ -> Expect.equal 4 (2 + 2))

interpTest : Test
interpTest = 
    describe "Interp"
        [test "Interp on NumC"
            (\_ -> Expect.equal "4" 
            (Main.serialize (Main.interp (Main.NumC { num = 4.0}) Main.topEnv))),
        test "Interp on StrC"
            (\_ -> Expect.equal "yes"
            (Main.serialize (Main.interp (Main.StrC { str = "yes"}) Main.topEnv))),
        test "Interp on IdC"
            (\_ -> Expect.equal (Main.BoolV {b = False})
            (Main.interp (Main.IdC {sym = "false"}) Main.topEnv)),
        test "Interp on IfC"
            (\_ -> Expect.equal "10"
            (Main.serialize (Main.interp (Main.IfC {cond = Main.IdC {sym = "true"},
                                                    thn = Main.NumC { num = 10.0},
                                                    els = Main.NumC { num = 25.0}})
                                                    Main.topEnv))),
        test "Interp on AppC" -- Equivalent to (top-interp '{{lam {x y} {+ x y}} {+ 4 5} 2})
            (\_ -> Expect.equal "11"
            (Main.serialize (Main.interp 
                (Main.AppC {func = Main.LamC {params = ["x", "y"],
                                                body = Main.AppC {func = Main.IdC {sym = "+"},
                                                                    args = [Main.IdC {sym = "x"},
                                                                            Main.IdC {sym = "y"}]}},
                            args = [Main.AppC {func = Main.IdC {sym = "+"},
                                                args = [Main.NumC { num = 4},
                                                        Main.NumC { num = 5}]},
                                    Main.NumC { num = 2}]})
                Main.topEnv)))]
    


