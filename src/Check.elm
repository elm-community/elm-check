module Check exposing (..)

{-|

A toolkit for writing property-based tests, which take the form of `Claim`s. A
`Claim` is made using the provided domain-specific language (DSL). A single
`Claim` can be written in one of these ways:

1. claim - (string) - that - (actual) - is - (expected) - for - (producer)
2. claim - (string) - true - (predicate) - for - (producer)
3. claim - (string) - false - (predicate) - for - (producer)


For example,

    claim_multiplication_identity =
      claim
        "Multiplying by one does not change a number"
      `that`
        (\n -> n * 1)
      `is`
        identity
      `for`
        int

See the README for more information.

*Warning: The DSL follows a very strict format. Deviating from this format will
yield potentially unintelligible type errors. The following functions have
horrendous type signatures and you are better off ignoring them.*

@docs claim, that, is, for, true, false

# Group Claims
@docs suite

# Check a Claim
@docs quickCheck, check

# Types
@docs Claim

## Evidence
The results of checking a claim are given back in the types defined here. You
can examine them yourself, or see `Check.Test` to convert them into tests to use
with `elm-check`'s runners.
@docs Evidence, UnitEvidence, SuccessOptions, FailureOptions
-}

import Lazy.List exposing (LazyList)
import Random exposing (Seed, Generator)
import Trampoline exposing (Trampoline(..))
import Check.Producer exposing (Producer)


{-| A Claim is an object that makes a claim of truth about a system.
A claim is either a function which yields evidence regarding the claim
or a list of such claims.
-}
type Claim
    = Claim String (Int -> Seed -> Evidence)
    | Suite String (List Claim)


{-| Evidence is the output from checking a claim or multiple claims.
-}
type Evidence
    = Unit UnitEvidence
    | Multiple String (List Evidence)


{-| UnitEvidence is the concrete type returned by checking a single claim.
A UnitEvidence can easily be converted to an assertion or can be considered
as the result of an assertion.
-}
type alias UnitEvidence =
    Result FailureOptions SuccessOptions


{-| SuccessOptions is the concrete type returned in case there is no evidence
found disproving a Claim.

SuccessOptions contains:
1. the `name` of the claim
2. the number of checks performed
3. the `seed` used in order to reproduce the check.
-}
type alias SuccessOptions =
    { name : String
    , seed : Seed
    , numberOfChecks : Int
    }


{-| FailureOptions is the concrete type returned in case evidence was found
disproving a Claim.

FailureOptions contains:
1. the `name` of the claim
2. the minimal `counterExample` which serves as evidence that the claim is false
3. the value `expected` to be returned by the claim
4. the `actual` value returned by the claim
5. the `seed` used in order to reproduce the results
6. the number of checks performed
7. the number of shrinking operations performed
8. the original `counterExample`, `actual`, and `expected` values found prior
to performing the shrinking operations.
-}
type alias FailureOptions =
    { name : String
    , counterExample : String
    , actual : String
    , expected : String
    , original :
        { counterExample : String
        , actual : String
        , expected : String
        }
    , seed : Seed
    , numberOfChecks : Int
    , numberOfShrinks : Int
    }


{-|
-}
claim : String -> (a -> b) -> (a -> b) -> Producer a -> Claim
claim name actualStatement expectedStatement producer =
    -------------------------------------------------------------------
    -- QuickCheck Algorithm with Shrinking :
    -- 1. Find a counter example within a given number of checks
    -- 2. If there is no such counter example, return a success
    -- 3. Else, shrink the counter example to a minimal representation
    -- 4. Return a failure.
    -------------------------------------------------------------------
    Claim name <|
        -- A Claim is just a function that takes a number of checks
        -- and a random seed and returns an `Evidence` object
        \numberOfChecks seed ->
            -- `numberOfChecks` is the given number of checks which is usually
            -- passed in by the `check` function. This sets an upper bound on
            -- the number of checks performed in order to find a counter example
            -- `seed` is the random seed which is usually passed in by the `check`
            -- function. Explictly passing random seeds allow the user to reproduce
            -- checks in order to re-run old checks on newer, presumably less buggy,
            -- code.
            let
                -- Find the original counter example. The original counter example
                -- is the first counter example found that disproves the claim.
                -- This counter example, if found, will later be shrunk into a more
                -- minimal version, hence "original".
                -- Note that since finding a counter example is a recursive process,
                -- trampolines are used. `findOriginalCounterExample` returns a
                -- trampoline.
                findOriginalCounterExample : Seed -> Int -> Trampoline (Result ( a, b, b, Seed, Int ) Int)
                findOriginalCounterExample seed currentNumberOfChecks =
                    if currentNumberOfChecks >= numberOfChecks then
                        ------------------------------------------------------------------
                        -- Stopping Condition:
                        -- If we have checked the claim at least `numberOfChecks` times
                        -- Then we simple return `Ok` with the number of checks signifying
                        -- that we have failed to find a counter example.
                        ------------------------------------------------------------------
                        Trampoline.done (Ok numberOfChecks)
                    else
                        let
                            --------------------------------------------------------------
                            -- Body of loop:
                            -- 1. We generate a new random value and the next seed using
                            --    the producer's random generator and the previous seed.
                            -- 2. We calculate the actual outcome and the expected
                            --    outcome from the given `actualStatement` and
                            --    `expectedStatement` respectively
                            -- 3. We compare the actual and the expected
                            -- 4. If actual equals expected, we continue the loop with
                            --    the next seed and incrementing the current number of
                            --    checks
                            -- 5. Else, we have found our counter example.
                            --------------------------------------------------------------
                            ( value, nextSeed ) =
                                Random.step producer.generator seed

                            actual =
                                actualStatement value

                            expected =
                                expectedStatement value
                        in
                            if actual == expected then
                                Trampoline.jump (\() -> findOriginalCounterExample nextSeed (currentNumberOfChecks + 1))
                            else
                                Trampoline.done (Err ( value, actual, expected, nextSeed, currentNumberOfChecks + 1 ))

                originalCounterExample : Result ( a, b, b, Seed, Int ) Int
                originalCounterExample =
                    Trampoline.evaluate (findOriginalCounterExample seed 0)
            in
                case originalCounterExample of
                    ------------------------------------------------------------
                    -- Case: No counter examples were found
                    -- We simply return the name of the claim, the seed, and the
                    -- number of checks performed.
                    ------------------------------------------------------------
                    Ok numberOfChecks ->
                        Unit <|
                            Ok
                                { name = name
                                , seed = seed
                                , numberOfChecks = max 0 numberOfChecks
                                }

                    ------------------------------------------------------------
                    -- Case : A counter example was found
                    -- We proceed to shrink the counter example to a more minimal
                    -- representation which still disproves the claim.
                    ------------------------------------------------------------
                    Err ( originalCounterExample, originalActual, originalExpected, seed, numberOfChecks ) ->
                        let
                            ------------------------------------------------------------------
                            -- Find the minimal counter example:
                            -- 1. Given a counter example, we produce a list of values
                            --    considered more minimal (i.e. we shrink the counter example)
                            -- 2. We keep only the shrunken values that disprove the claim.
                            -- 3. If there are no such shrunken value, then we consider the
                            --    given counter example to be minimal and report the number
                            --    of shrinking operations performed.
                            -- 4. Else, we recurse, passing in the new shrunken value
                            --    and incrementing the current number of shrinks counter.
                            ------------------------------------------------------------------
                            -- Note that since finding the minimal counter example is a
                            -- recursive process, trampolines are used. `shrink` returns
                            -- a trampoline.
                            -- shrink : a -> Int -> Trampoline (a, Int)
                            shrink counterExample currentNumberOfShrinks =
                                let
                                    -- Produce a list of values considered more minimal that
                                    -- the given `counterExample`.
                                    shrunkenCounterExamples : LazyList a
                                    shrunkenCounterExamples =
                                        producer.shrinker counterExample

                                    -- Keep only the counter examples that disprove the claim.
                                    -- (i.e. they violate `actual == expected`)
                                    failingShrunkenCounterExamples : LazyList a
                                    failingShrunkenCounterExamples =
                                        Lazy.List.keepIf
                                            (\shrunk ->
                                                not (actualStatement shrunk == expectedStatement shrunk)
                                            )
                                            shrunkenCounterExamples
                                in
                                    case Lazy.List.head failingShrunkenCounterExamples of
                                        Nothing ->
                                            --------------------------------------------------------
                                            -- Stopping Condition :
                                            -- If there are no further shrunken counter examples
                                            -- we simply return the given counter example and report
                                            -- the number of shrinking operations performed.
                                            --------------------------------------------------------
                                            Trampoline.done ( counterExample, currentNumberOfShrinks )

                                        Just failing ->
                                            --------------------------------------------------------
                                            -- Body of Loop :
                                            -- We simply recurse with the first shrunken counter
                                            -- example we can get our hands on and incrementing the
                                            -- current number of shrinking operations counter
                                            --------------------------------------------------------
                                            Trampoline.jump (\() -> shrink failing (currentNumberOfShrinks + 1))

                            -- minimal : a
                            -- numberOfShrinks : Int
                            ( minimal, numberOfShrinks ) =
                                Trampoline.evaluate (shrink originalCounterExample 0)

                            actual : b
                            actual =
                                actualStatement minimal

                            expected : b
                            expected =
                                expectedStatement minimal
                        in
                            -- Here, we return an `Err` signifying that a counter example was
                            -- found. The returned record contains a number of fields and
                            -- values useful for diagnostics, such as the counter example,
                            -- the expected and the actual values, as well the original
                            -- unshrunk versions, the name of the claim, the seed used to
                            -- find the counter example, the number of checks performed to find
                            -- the counter example, and the number of shrinking operations
                            -- performed.
                            Unit <|
                                Err
                                    { name = name
                                    , seed = seed
                                    , counterExample = toString minimal
                                    , expected = toString expected
                                    , actual = toString actual
                                    , original =
                                        { counterExample = toString originalCounterExample
                                        , actual = toString originalActual
                                        , expected = toString originalExpected
                                        }
                                    , numberOfChecks = numberOfChecks
                                    , numberOfShrinks = numberOfShrinks
                                    }


{-| Check a claim and produce evidence.

To check a claim, you need to provide the number of checks to perform, and a
random seed. You can set up a CI server to run through a large number of checks
with a randomized seed.

    aggressiveCheck : Claim -> Evidence
    aggressiveCheck =
      check 2000 (Random.initialSeed 0xFFFF)
-}
check : Int -> Seed -> Claim -> Evidence
check n seed claim =
    case claim of
        Claim name f ->
            f n seed

        Suite name claims ->
            Multiple name (List.map (check n seed) claims)


{-| Quickly check a claim.

This function is very useful when checking claims in local development.
`quickCheck` will perform 100 checks and use `Random.initialSeed 1` as the
random seed.
-}
quickCheck : Claim -> Evidence
quickCheck =
    check 100 (Random.initialSeed 1)


{-| Group a list of claims into a suite. This is very useful in order to
group similar claims together.

    suite nameOfSuite listOfClaims

Suites can be nested as deep as you like.

    suite "All tests"
      [ someClaim
      , suite "Regression tests" listOfClaims
      ]
-}
suite : String -> List Claim -> Claim
suite name claims =
    Suite name claims


{-| -}
that : ((a -> b) -> (a -> b) -> Producer a -> Claim) -> (a -> b) -> ((a -> b) -> Producer a -> Claim)
that f x =
    f x


{-| -}
is : ((a -> b) -> Producer a -> Claim) -> (a -> b) -> (Producer a -> Claim)
is f x =
    f x


{-| -}
for : (Producer a -> Claim) -> Producer a -> Claim
for f x =
    f x


{-| -}
true : ((a -> Bool) -> (a -> Bool) -> Producer a -> Claim) -> (a -> Bool) -> (Producer a -> Claim)
true f pred =
    f pred (always True)


{-| -}
false : ((a -> Bool) -> (a -> Bool) -> Producer a -> Claim) -> (a -> Bool) -> (Producer a -> Claim)
false f pred =
    f pred (always False)
