module Check.Producer (..) where

{-| Sub-module containing the Producer type used by elm-check.

This sub-module contains several predefined producer generators and means of
composing them to create your own. Note that most generators provided are
only well-suited to local development. Property-based testing is by its nature
a very slow process and is best paired with some sort of continuous integration
service. Consider making your own, more general producer generators when
migrating from local to cloud-based.

# Producer Definition
@docs Producer

# Basic Producer Generators
@docs void, bool, order, int, rangeInt, float, percentage, char, upperCaseChar, lowerCaseChar, ascii, unicode, string, maybe, result, list, array, tuple, tuple3, tuple4, tuple5, func, func2, func3, func4, func5

# Working with Producers
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


{-| An Producer type is a Random Generator paired with a shrinking strategy,
or Shrinker. Shrinkers are defined in `elm-shrink`.
-}
type alias Producer a =
  { generator : Generator a
  , shrinker : Shrinker a
  }


{-| Producer void. Uses a constant generator and the `void` shrinker from
elm-shrink.
-}
void : Producer ()
void =
  Producer (Random.Extra.constant ()) Shrink.void


{-| Producer bool. Uses the bool generator from elm-random-extra and the
`bool` shrinker from elm-shrink.
-}
bool : Producer Bool
bool =
  Producer (Random.Bool.bool) Shrink.bool


{-| Producer order. Uses the order generator from elm-random-extra and the
`order` shrinker from elm-shrink.
-}
order : Producer Order
order =
  Producer (Random.Order.order) Shrink.order


{-| Producer int. Generates random ints between -50 and 50 and the `int`
shrinker from elm-shrink. Ideal for local testing.
-}
int : Producer Int
int =
  let
    generator =
      Random.Extra.frequency
        [ ( 3, Random.int -50 50 )
        , ( 1, Random.int Random.minInt Random.maxInt )
        ]
        (Random.int -50 50)
  in
    Producer generator Shrink.int


{-| Producer int constructor. Generates random ints between a given `min`
and a given `max` value.
-}
rangeInt : Int -> Int -> Producer Int
rangeInt min max =
  Producer (Random.int min max) Shrink.int


{-| Producer float. Generates random floats between -50 and 50 and the `float`
shrinker from elm-shrink. Ideal for local testing.
-}
float : Producer Float
float =
  let
    generator =
      Random.Extra.frequency
        [ ( 3, Random.float -50 50 )
        , ( 1, Random.float (toFloat Random.minInt) (toFloat Random.maxInt) )
        ]
        (Random.float -50 50)
  in
    Producer (Random.float -50 50) Shrink.float


{-| Producer percentage. Generates random floats between 0.0 and 1.0 and the `float`
shrinker from elm-shrink. Useful in conjunction with `tuple` to facilitate
things like generating an array and then selecting one of its elements at random.
-}
percentage : Producer Float
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
    Producer generator Shrink.float


{-| Producer char. Generates random ascii chars using the `ascii` generator
from elm-random-extra and the `char` shrinker from elm-shrink. Ideal for local
testing or if your domain deals exclusively with ascii.
-}
ascii : Producer Char
ascii =
  Producer (Random.Char.ascii) Shrink.char


{-| Producer char. Generates random ascii chars disregarding the control
characters using the `char 32 127` generator from elm-random-extra and the
`character` shrinker from elm-shrink. Ideal for local testing or if your
domain deals exclusively with ascii and you do not care about control
characters.
-}
char : Producer Char
char =
  Producer (Random.Char.char 32 127) Shrink.character


{-| Producer char. Generates random ascii chars using the `upperCaseLatin`
generator from elm-random-extra and the `character` shrinker from elm-shrink.
-}
upperCaseChar : Producer Char
upperCaseChar =
  Producer Random.Char.upperCaseLatin Shrink.character


{-| Producer char. Generates random ascii chars using the `lowerCaseLatin`
generator from elm-random-extra and the `character` shrinker from elm-shrink.
-}
lowerCaseChar : Producer Char
lowerCaseChar =
  Producer Random.Char.lowerCaseLatin Shrink.character


{-| Producer char. Generates a random UTF-8 character using the
`unicode` generator from elm-random-extra and the `char` shrinker from
elm-shrink.
-}
unicode : Producer Char
unicode =
  Producer (Random.Char.unicode) Shrink.char


{-| Producer string. Generates random ascii strings of size between 0 and 10
using the `rangeLengthString` generator from elm-random-extra and the `string`
shrinker from elm-shrink. Ideal for local testing.
-}
string : Producer String
string =
  Producer
    (Random.String.rangeLengthString 0 10 Random.Char.ascii)
    (Shrink.string)


{-| Producer maybe constructor. Generates random maybe values from a given
producer generator using the `maybe` generator constructor from
elm-random-extra and the `maybe` shrinker constructor from elm-shrink.
-}
maybe : Producer a -> Producer (Maybe a)
maybe prod =
  Producer
    (Random.Maybe.maybe prod.generator)
    (Shrink.maybe prod.shrinker)


{-| Producer result constructor. Generates random result values from a given
producer generator using the `result` generator constructor from
elm-random-extra and the `result` shrinker constrctor from elm-shrink.
-}
result : Producer error -> Producer value -> Producer (Result error value)
result errSpec valSpec =
  Producer
    (Random.Result.result errSpec.generator valSpec.generator)
    (Shrink.result errSpec.shrinker valSpec.shrinker)


{-| Producer list constructor. Generates random lists of values of size
between 0 and 10 from a given producer generator using the `rangeLengthList`
generator constructor from elm-random-extra and the `list` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
list : Producer a -> Producer (List a)
list prod =
  Producer
    (Random.List.rangeLengthList 0 10 prod.generator)
    (Shrink.list prod.shrinker)


{-| Producer array constructor. Generates random arrays of values of size
between 0 and 10 from a given producer generator using the `rangeLengthArray`
generator constructor from elm-random-extra and the `array` shrinker constructor
from elm-shrink. Ideal for local testing.
-}
array : Producer a -> Producer (Array a)
array prod =
  Producer
    (Random.Array.rangeLengthArray 0 10 prod.generator)
    (Shrink.array prod.shrinker)


{-| Producer 2-tuple constructor. Generates random 2-tuples from a 2-tuple
of producer generators. Uses the `tuple` shrinker constructor from elm-shrink.
-}
tuple : ( Producer a, Producer b ) -> Producer ( a, b )
tuple ( prodA, prodB ) =
  Producer
    (Random.Extra.zip prodA.generator prodB.generator)
    (Shrink.tuple ( prodA.shrinker, prodB.shrinker ))


{-| Producer 3-tuple constructor. Generates random 3-tuples from a 3-tuple
of producer generators. Uses the `tuple3` shrinker constrctor from elm-shrink.
-}
tuple3 : ( Producer a, Producer b, Producer c ) -> Producer ( a, b, c )
tuple3 ( prodA, prodB, prodC ) =
  Producer
    (Random.Extra.zip3 prodA.generator prodB.generator prodC.generator)
    (Shrink.tuple3 ( prodA.shrinker, prodB.shrinker, prodC.shrinker ))


{-| Producer 4-tuple constructor. Generates random 4-tuples from a 4-tuple
of producer generators. Uses the `tuple4` shrinker constrctor from elm-shrink.
-}
tuple4 : ( Producer a, Producer b, Producer c, Producer d ) -> Producer ( a, b, c, d )
tuple4 ( prodA, prodB, prodC, prodD ) =
  Producer
    (Random.Extra.zip4 prodA.generator prodB.generator prodC.generator prodD.generator)
    (Shrink.tuple4 ( prodA.shrinker, prodB.shrinker, prodC.shrinker, prodD.shrinker ))


{-| Producer 5-tuple constructor. Generates random 5-tuples from a 5-tuple
of producer generators. Uses the `tuple5` shrinker constrctor from elm-shrink.
-}
tuple5 : ( Producer a, Producer b, Producer c, Producer d, Producer e ) -> Producer ( a, b, c, d, e )
tuple5 ( prodA, prodB, prodC, prodD, prodE ) =
  Producer
    (Random.Extra.zip5 prodA.generator prodB.generator prodC.generator prodD.generator prodE.generator)
    (Shrink.tuple5 ( prodA.shrinker, prodB.shrinker, prodC.shrinker, prodD.shrinker, prodE.shrinker ))


{-| Filter out an Producer. The resulting Producer will only generate
random test values or shrunken values that satisfy the predicate. Uses the
`keepIf` filter from elm-random-extra and the `keepIf` filter from elm-shrink.
-}
keepIf : (a -> Bool) -> Producer a -> Producer a
keepIf predicate prod =
  Producer
    (Random.Extra.keepIf predicate prod.generator)
    (Shrink.keepIf predicate prod.shrinker)


{-| Filter out an Producer. The resulting Producer will only generate
random test values or shrunken values that do not satisfy the predicate. Uses
the `dropIf` filter from elm-random-extra and the `dropIf` filter from
elm-shrink.
-}
dropIf : (a -> Bool) -> Producer a -> Producer a
dropIf predicate prod =
  Producer
    (Random.Extra.dropIf predicate prod.generator)
    (Shrink.dropIf predicate prod.shrinker)


{-| Convert the output of one producer to another type. This is useful if
you're testing a function that expects a large model record, but you only need
to randomize a few fields. You might do this several different ways for a single
model, so you generate and shrink only the fields relevant to each test.

    type alias Person =
      { first : String, last : String, age : String }

    spy : Producer Person
    spy = map (\age -> Person "James" "Bond" age) .age (rangeInt 0 120)

In order for shrinking to work, you need to pass an inverse function of the
function being mapped.
-}
convert : (a -> b) -> (b -> a) -> Producer a -> Producer b
convert f g prod =
  Producer
    (Random.map f prod.generator)
    (Shrink.convert f g prod.shrinker)

{-| Map a function over an producer. This works exactly like `convert`,
except it does not require an invderse function, and consequently does no
shrinking.
-}
map : (a -> b) -> Producer a -> Producer b
map f prod =
  Producer
    (Random.map f prod.generator)
    Shrink.noShrink


{-| Producer of functions. Takes an producer for the return type
and returns an producer of functions. Uses the `func` generator from
elm-random-extra and does not perform any shrinking.
-}
func : Producer b -> Producer (a -> b)
func prodB =
  Producer
    (Random.Function.func prodB.generator)
    (Shrink.noShrink)


{-| -}
func2 : Producer c -> Producer (a -> b -> c)
func2 prodC =
  Producer
    (Random.Function.func2 prodC.generator)
    (Shrink.noShrink)


{-| -}
func3 : Producer d -> Producer (a -> b -> c -> d)
func3 prodD =
  Producer
    (Random.Function.func3 prodD.generator)
    (Shrink.noShrink)


{-| -}
func4 : Producer e -> Producer (a -> b -> c -> d -> e)
func4 prodE =
  Producer
    (Random.Function.func4 prodE.generator)
    (Shrink.noShrink)


{-| -}
func5 : Producer f -> Producer (a -> b -> c -> d -> e -> f)
func5 prodF =
  Producer
    (Random.Function.func5 prodF.generator)
    (Shrink.noShrink)
