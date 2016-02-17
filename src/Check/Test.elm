module Check.Test (evidenceToTest) where

{-| Submodule providing integration with elm-test.

# Convert to tests
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
      ElmTest.test name
        <| ElmTest.fail
        <| "On check "
        ++ toString numberOfChecks
        ++ ", found counterexample: "
        ++ counterExample
        ++ " Expected "
        ++ expected
        ++ " but got "
        ++ actual


nChecks : Int -> String
nChecks n =
  if n == 1 then
    "1 check"
  else
    toString n ++ " checks"
