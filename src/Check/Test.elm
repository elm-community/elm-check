module Check.Test (evidenceToTest) where

{-| This module provides integration with
[`elm-test`](http://package.elm-lang.org/packages/deadfoxygrandpa/elm-test/latest/).

# Convert to Tests
@docs evidenceToTest

-}

import Check
import ElmTest


{-| Convert elm-check's Evidence into an elm-test Test. You can use elm-test's
runners to view the results of your property-based tests, alongside the results
of unit tests.
-}
evidenceToTest : Check.Evidence -> ElmTest.Test
evidenceToTest evidence =
  case evidence of
    Check.Multiple name more ->
      ElmTest.suite name (List.map evidenceToTest more)

    Check.Unit (Ok { name, numberOfChecks }) ->
      ElmTest.test (name ++ " [" ++ nChecks numberOfChecks ++ "]") ElmTest.pass

    Check.Unit (Err { name, numberOfChecks, expected, actual, counterExample }) ->
      ElmTest.test name <| ElmTest.fail
        <| "\nOn check " ++ toString numberOfChecks ++ ", found counterexample: "
        ++ counterExample ++ "\nExpected:   " ++ expected ++ "\nBut It Was: " ++ actual


nChecks : Int -> String
nChecks n =
  if n == 1 then
    "1 check"
  else
    toString n ++ " checks"
