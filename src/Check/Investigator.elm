module Check.Investigator (..) where

{-| Sub-module containing the Investigator type used by elm-check.

This sub-module contains several predefined investigator generators and means of
composing them to create your own. Note that most generators provided are
only well-suited to local development. Property-based testing is by its nature
a very slow process and is best paired with some sort of continuous integration
service. Consider making your own, more general investigator generators when
migrating from local to cloud-based.

# Investigator Definition
@docs Investigator

# Basic Investigator Generators
@docs void, bool, order, int, rangeInt, float, percentage, char, upperCaseChar, lowerCaseChar, ascii, unicode, string, maybe, result, list, array, tuple, tuple3, tuple4, tuple5, func, func2, func3, func4, func5

# Working with Investigators
@docs convert, map, keepIf, dropIf

-}

import Array exposing (Array)
import Shrink exposing (Shrinker)
import Random exposing (Generator)
import Random.Extra
import Random.Bool
import Random.Function
import Random.Order
import Random.Char
import Random.String
import Random.Maybe
import Random.Result
import Random.List
import Random.Array


{-| An Investigator type is a Random Generator paired with a shrinking strategy,
or Shrinker. Shrinkers are defined in `elm-shrink`.
-}
type alias Investigator a =
  { generator : Generator a
  , shrinker : Shrinker a
  }


{-| Investigator void. Uses a constant generator and the `void` shrinker from
elm-shrink.
-}
void : Investigator ()
void =
  Investigator (Random.Extra.constant ()) Shrink.void


{-| Investigator bool. Uses the bool generator from elm-random-extra and the
`bool` shrinker from elm-shrink.
-}
bool : Investigator Bool
bool =
  Investigator (Random.Bool.bool) Shrink.bool


{-| Investigator order. Uses the order generator from elm-random-extra and the
`order` shrinker from elm-shrink.
-}
order : Investigator Order
order =
  Investigator (Random.Order.order) Shrink.order


{-| Investigator int. Generates random ints between -50 and 50 and the `int`
shrinker from elm-shrink. Ideal for local testing.
-}
int : Investigator Int
int =
  let
    generator =
      Random.Extra.frequency
        [ ( 3, Random.int -50 50 )
        , ( 1, Random.int Random.minInt Random.maxInt )
        ]
        (Random.int -50 50)
  in
    Investigator generator Shrink.int


{-| Investigator int constructor. Generates random ints between a given `min`
and a given `max` value.
-}
rangeInt : Int -> Int -> Investigator Int
rangeInt min max =
  Investigator (Random.int min max) Shrink.int


{-| Investigator float. Generates random floats between -50 and 50 and the `float`
shrinker from elm-shrink. Ideal for local testing.
-}
float : Investigator Float
float =
  let
    generator =
      Random.Extra.frequency
        [ ( 3, Random.float -50 50 )
        , ( 1, Random.float (toFloat Random.minInt) (toFloat Random.maxInt) )
        ]
        (Random.float -50 50)
  in
    Investigator (Random.float -50 50) Shrink.float


{-| Investigator percentage. Generates random floats between 0.0 and 1.0 and the `float`
shrinker from elm-shrink. Useful in conjunction with `tuple` to facilitate
things like generating an array and then selecting one of its elements at random.
-}
percentage : Investigator Float
percentage =
  let
    generator =
      Random.Extra.frequency
        [ ( 3, Random.float 0 1 )
        , ( 1, Random.Extra.constant 0 )
        , ( 1, Random.Extra.constant 1 )
        ]
        (Random.float 0 1)
  in
    Investigator generator Shrink.float


{-| Investigator char. Generates random ascii chars using the `ascii` generator
from elm-random-extra and the `char` shrinker from elm-shrink. Ideal for local
testing or if your domain deals exclusively with ascii.
-}
ascii : Investigator Char
ascii =
  Investigator (Random.Char.ascii) Shrink.char


{-| Investigator char. Generates random ascii chars disregarding the control
characters using the `char 32 127` generator from elm-random-extra and the
`character` shrinker from elm-shrink. Ideal for local testing or if your
domain deals exclusively with ascii and you do not care about control
characters.
-}
char : Investigator Char
char =
  Investigator (Random.Char.char 32 127) Shrink.character


{-| Investigator char. Generates random ascii chars using the `upperCaseLatin`
generator from elm-random-extra and the `character` shrinker from elm-shrink.
-}
upperCaseChar : Investigator Char
upperCaseChar =
  Investigator Random.Char.upperCaseLatin Shrink.character


{-| Investigator char. Generates random ascii chars using the `lowerCaseLatin`
generator from elm-random-extra and the `character` shrinker from elm-shrink.
-}
lowerCaseChar : Investigator Char
lowerCaseChar =
  Investigator Random.Char.lowerCaseLatin Shrink.character


{-| Investigator char. Generates a random UTF-8 character using the
`unicode` generator from elm-random-extra and the `char` shrinker from
elm-shrink.
-}
unicode : Investigator Char
unicode =
  Investigator (Random.Char.unicode) Shrink.char


{-| Investigator string. Generates random ascii strings of size between 0 and 10
using the `rangeLengthString` generator from elm-random-extra and the `string`
shrinker from elm-shrink. Ideal for local testing.
-}
string : Investigator String
string =
  Investigator
    (Random.String.rangeLengthString 0 10 Random.Char.ascii)
    (Shrink.string)


{-| Investigator maybe constructor. Generates random maybe values from a given
investigator generator using the `maybe` generator constructor from
elm-random-extra and the `maybe` shrinker constructor from elm-shrink.
-}
maybe : Investigator a -> Investigator (Maybe a)
maybe inv =
  Investigator
    (Random.Maybe.maybe inv.generator)
    (Shrink.maybe inv.shrinker)


{-| Investigator result constructor. Generates random result values from a given
investigator generator using the `result` generator constructor from
elm-random-extra and the `result` shrinker constrctor from elm-shrink.
-}
result : Investigator error -> Investigator value -> Investigator (Result error value)
result errSpec valSpec =
  Investigator
    (Random.Result.result errSpec.generator valSpec.generator)
    (Shrink.result errSpec.shrinker valSpec.shrinker)


{-| Investigator list constructor. Generates random lists of values of size
between 0 and 10 from a given investigator generator using the `rangeLengthList`
generator constructor from elm-random-extra and the `list` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
list : Investigator a -> Investigator (List a)
list inv =
  Investigator
    (Random.List.rangeLengthList 0 10 inv.generator)
    (Shrink.list inv.shrinker)


{-| Investigator array constructor. Generates random arrays of values of size
between 0 and 10 from a given investigator generator using the `rangeLengthArray`
generator constructor from elm-random-extra and the `array` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
array : Investigator a -> Investigator (Array a)
array inv =
  Investigator
    (Random.Array.rangeLengthArray 0 10 inv.generator)
    (Shrink.array inv.shrinker)


{-| Investigator 2-tuple constructor. Generates random 2-tuples from a 2-tuple
of investigator generators. Uses the `tuple` shrinker constructor from elm-shrink.
-}
tuple : ( Investigator a, Investigator b ) -> Investigator ( a, b )
tuple ( invA, invB ) =
  Investigator
    (Random.Extra.zip invA.generator invB.generator)
    (Shrink.tuple ( invA.shrinker, invB.shrinker ))


{-| Investigator 3-tuple constructor. Generates random 3-tuples from a 3-tuple
of investigator generators. Uses the `tuple3` shrinker constrctor from elm-shrink.
-}
tuple3 : ( Investigator a, Investigator b, Investigator c ) -> Investigator ( a, b, c )
tuple3 ( invA, invB, invC ) =
  Investigator
    (Random.Extra.zip3 invA.generator invB.generator invC.generator)
    (Shrink.tuple3 ( invA.shrinker, invB.shrinker, invC.shrinker ))


{-| Investigator 4-tuple constructor. Generates random 4-tuples from a 4-tuple
of investigator generators. Uses the `tuple4` shrinker constrctor from elm-shrink.
-}
tuple4 : ( Investigator a, Investigator b, Investigator c, Investigator d ) -> Investigator ( a, b, c, d )
tuple4 ( invA, invB, invC, invD ) =
  Investigator
    (Random.Extra.zip4 invA.generator invB.generator invC.generator invD.generator)
    (Shrink.tuple4 ( invA.shrinker, invB.shrinker, invC.shrinker, invD.shrinker ))


{-| Investigator 5-tuple constructor. Generates random 5-tuples from a 5-tuple
of investigator generators. Uses the `tuple5` shrinker constrctor from elm-shrink.
-}
tuple5 : ( Investigator a, Investigator b, Investigator c, Investigator d, Investigator e ) -> Investigator ( a, b, c, d, e )
tuple5 ( invA, invB, invC, invD, invE ) =
  Investigator
    (Random.Extra.zip5 invA.generator invB.generator invC.generator invD.generator invE.generator)
    (Shrink.tuple5 ( invA.shrinker, invB.shrinker, invC.shrinker, invD.shrinker, invE.shrinker ))


{-| Filter out an Investigator. The resulting Investigator will only generate
random test values or shrunken values that satisfy the predicate. Uses the
`keepIf` filter from elm-random-extra and the `keepIf` filter from elm-shrink.
-}
keepIf : (a -> Bool) -> Investigator a -> Investigator a
keepIf predicate inv =
  Investigator
    (Random.Extra.keepIf predicate inv.generator)
    (Shrink.keepIf predicate inv.shrinker)


{-| Filter out an Investigator. The resulting Investigator will only generate
random test values or shrunken values that do not satisfy the predicate. Uses
the `dropIf` filter from elm-random-extra and the `dropIf` filter from
elm-shrink.
-}
dropIf : (a -> Bool) -> Investigator a -> Investigator a
dropIf predicate inv =
  Investigator
    (Random.Extra.dropIf predicate inv.generator)
    (Shrink.dropIf predicate inv.shrinker)


{-| Convert the output of one investigator to another type. This is useful if
you're testing a function that expects a large model record, but you only need
to randomize a few fields. You might do this several different ways for a single
model, so you generate and shrink only the fields relevant to each test.

    type alias Person =
      { first : String, last : String, age : String }

    spy : Investigator Person
    spy = map (\age -> Person "James" "Bond" age) .age (rangeInt 0 120)

In order for shrinking to work, you need to pass an inverse function of the
function being mapped.
-}
convert : (a -> b) -> (b -> a) -> Investigator a -> Investigator b
convert f g inv =
  Investigator
    (Random.map f inv.generator)
    (Shrink.convert f g inv.shrinker)

{-| Map a function over an investigator. This works exactly like `convert`,
except it does not require an inverse function, and consequently does no
shrinking.
-}
map : (a -> b) -> Investigator a -> Investigator b
map f inv =
  Investigator
    (Random.map f inv.generator)
    Shrink.noShrink


{-| Investigator of functions. Takes an investigator for the return type
and returns an investigator of functions. Uses the `func` generator from
elm-random-extra and does not perform any shrinking.
-}
func : Investigator b -> Investigator (a -> b)
func invB =
  Investigator
    (Random.Function.func invB.generator)
    (Shrink.noShrink)


{-| -}
func2 : Investigator c -> Investigator (a -> b -> c)
func2 invC =
  Investigator
    (Random.Function.func2 invC.generator)
    (Shrink.noShrink)


{-| -}
func3 : Investigator d -> Investigator (a -> b -> c -> d)
func3 invD =
  Investigator
    (Random.Function.func3 invD.generator)
    (Shrink.noShrink)


{-| -}
func4 : Investigator e -> Investigator (a -> b -> c -> d -> e)
func4 invE =
  Investigator
    (Random.Function.func4 invE.generator)
    (Shrink.noShrink)


{-| -}
func5 : Investigator f -> Investigator (a -> b -> c -> d -> e -> f)
func5 invF =
  Investigator
    (Random.Function.func5 invF.generator)
    (Shrink.noShrink)
