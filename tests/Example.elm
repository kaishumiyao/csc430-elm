module Example exposing (..)

import Main as Main
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
    test "two plus two equals four"
        (\_ -> Expect.equal 4 (2 + 2))

numTest : Test
numTest = 
    test "Interp on NumC"
        (\_ -> Expect.equal "4" 
        (Main.serialize (Main.interp (Main.NumC { num = 4.0}) Main.topEnv)))

lookUpTest : Test
lookUpTest =
    test "Lookup on Symbol"
        (\_ -> Expect.equal (Main.BoolV {b = True})
        (Main.envlookup "true"  Main.topEnv))


