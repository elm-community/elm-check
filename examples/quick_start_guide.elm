module Main exposing (..)

import List exposing (reverse, length)
import Check exposing (Claim, Evidence, suite, claim, that, is, for, quickCheck)
import Check.Producer exposing (list, int)
import Check.Test
import ElmTest


myClaims : Claim
myClaims =
    suite "List Reverse"
        [ claim "Reversing a list twice yields the original list"
            `that` (\list -> reverse (reverse list))
            `is` identity
            `for` list int
        , claim "Reversing a list does not modify its length"
            `that` (\list -> length (reverse list))
            `is` (\list -> length list)
            `for` list int
        ]


evidence : Evidence
evidence =
    quickCheck myClaims


main =
    ElmTest.runSuite (Check.Test.evidenceToTest evidence)
