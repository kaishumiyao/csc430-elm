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


parseTest : Test
parseTest =
    describe "Parse"
        [test "Parse on number"
            (\_ -> Expect.equal (Main.NumC {num = 4.0})
            (Main.parse ["4"])),
        test "Parse on symbol"
            (\_ -> Expect.equal (Main.IdC {sym = "foo"})
            (Main.parse ["'foo"])),
        test "Parse on string"
            (\_ -> Expect.equal (Main.StrC {str = "myString"})
            (Main.parse ["myString"])),
        test "Parse on if statement"
            (\_ -> Expect.equal (Main.IfC {cond = (Main.NumC {num = 1.0}),
                                            thn = (Main.NumC {num = 2.0}),
                                            els = (Main.NumC {num = 3.0})})
            (Main.parse ["if", "1", "2", "3"])),
        test "Parse on if statement, all types"
            (\_ -> Expect.equal (Main.IfC {cond = (Main.IdC {sym = "true"}),
                                            thn = (Main.StrC {str = "foo"}),
                                            els = (Main.NumC {num = 2.0})})
            (Main.parse ["if", "'true", "foo", "2.0"]))]


parseAndInterpTest : Test
parseAndInterpTest =
    describe "Parse and interp"
        [test "Parse and interp on number"
            (\_ -> Expect.equal "4"
            (Main.serialize (Main.interp (Main.parse ["4"]) Main.topEnv))),
        test "Parse and interp on string"
            (\_ -> Expect.equal "foo"
            (Main.serialize (Main.interp (Main.parse ["foo"]) Main.topEnv))),
        test "Parse and interp on if, true"
            (\_ -> Expect.equal "2"
            (Main.serialize (Main.interp (Main.parse ["if", "'true", "2.0", "3.0"]) Main.topEnv))),
        test "Parse and interp on if, false"
            (\_ -> Expect.equal "falseClause"
            (Main.serialize (Main.interp (Main.parse ["if", "'false", "2.0", "falseClause"]) Main.topEnv))),
        test "Parse and interp on if"
            (\_ -> Expect.equal "foo"
            (Main.serialize (Main.interp (Main.parse ["if", "'true", "foo", "2.0"]) Main.topEnv)))]



