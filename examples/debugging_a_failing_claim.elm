module Main exposing (..)

import Check exposing (Claim, Evidence, suite, claim, that, is, for, true, quickCheck)
import Check.Producer exposing (tuple, float, filter)
import Check.Test
import ElmTest


testWithZero : Claim
testWithZero =
    claim "Multiplication and division are inverse operations"
        `that` (\( x, y ) -> x * y / y)
        `is` (\( x, y ) -> x)
        `for` tuple ( float, float )


testWithoutZero : Claim
testWithoutZero =
    claim "Multiplication and division are inverse operations, if zero is omitted"
        `that` (\( x, y ) -> x * y / y)
        `is` (\( x, y ) -> x)
        `for` filter (\( x, y ) -> y /= 0) (tuple ( float, float ))


testForNearness : Claim
testForNearness =
    claim "Multiplication and division are near inverse operations, if zero is omitted"
        `true` (\( x, y ) -> abs ((x * y / y) - x) < 1.0e-6)
        `for` filter (\( x, y ) -> y /= 0) (tuple ( float, float ))


myClaims : Claim
myClaims =
    suite "Claims about multiplication and division"
        [ testWithZero, testWithoutZero, testForNearness ]


evidence : Evidence
evidence =
    quickCheck myClaims


main =
    ElmTest.runSuite (Check.Test.evidenceToTest evidence)
