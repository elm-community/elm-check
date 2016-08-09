module Check.Test exposing (evidenceToTest)

{-| This module provides integration with
[`elm-test`](http://package.elm-lang.org/packages/deadfoxygrandpa/elm-test/latest/).

# Convert to Tests
@docs evidenceToTest

-}

import Check
import Test exposing (Test)
import Expect


{-| Convert elm-check's Evidence into an elm-test Test. You can use elm-test's
runners to view the results of your property-based tests, alongside the results
of unit tests.
-}
evidenceToTest : Check.Evidence -> Test
evidenceToTest evidence =
    case evidence of
        Check.Multiple name more ->
            Test.describe name (List.map evidenceToTest more)

        Check.Unit (Ok { name, numberOfChecks }) ->
            Test.test (name ++ " [" ++ nChecks numberOfChecks ++ "]") <|
                \() -> Expect.pass

        Check.Unit (Err { name, numberOfChecks, expected, actual, counterExample }) ->
            Test.test name <|
                \() ->
                    Expect.fail <|
                        "\nOn check "
                            ++ toString numberOfChecks
                            ++ ", found counterexample: "
                            ++ counterExample
                            ++ "\nExpected:   "
                            ++ expected
                            ++ "\nBut It Was: "
                            ++ actual


nChecks : Int -> String
nChecks n =
    if n == 1 then
        "1 check"
    else
        toString n ++ " checks"
