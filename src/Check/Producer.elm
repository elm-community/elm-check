module Check.Producer exposing (..)

{-| This is a library of `Producer`s you can use to supply values to your tests.
You can typically pick out which ones you need according to their types.

A `Producer a` knows how to create values of type `a`. It can create them
randomly, and it can shrink them to more minimal values. Producers can be
filtered and mapped over.

# Common Producers
@docs bool, int, rangeInt, float, rangeFloat, percentage, string, maybe, result, list, array

## Tuple Producers
If your expected and actual functions need more than one input, pass them in as a tuple.
@docs tuple, tuple3, tuple4, tuple5

# Working with Producers
@docs Producer, filter, convert, map

# Uncommon Producers
@docs unit, order

## Character Producers
@docs char, upperCaseChar, lowerCaseChar, ascii, unicode

-}

import Array exposing (Array)
import Shrink exposing (Shrinker)
import Random exposing (Generator)
import Random.Extra
import Random.Order
import Random.Char
import Random.String
import Random.Array


{-| An Producer type is a
[Random](http://package.elm-lang.org/packages/elm-lang/core/latest/Random)
`Generator` paired with a shrinking strategy, or `Shrinker`. Shrinkers are defined
in [`elm-community/shrink`](http://package.elm-lang.org/packages/elm-community/shrink/latest/).
You will need to be familiar with both libraries to write custom producers for your own types.
Here is an example for a record:

    type alias Position =
        { x : Int, y : Int }


    position : Producer Position
    position =
        Producer
            (Random.map2 Position (Random.int 0 1919) (Random.int 0 1079))
            (\{ x, y } -> Shrink.map Position (Shrink.int x) `Shrink.andMap` (Shrink.int y))

Here is an example for a union type:

    type Question
        = Name String
        | Age Int


    question =
        let
            generator =
                Random.bool `Random.andThen` (\b ->
                    if b then
                        Random.map Name string.generator
                    else
                        Random.map Age (Random.int 0 120)
                 )

            shrinker question =
                case question of
                    Name n ->
                        Shrink.string n |> Shrink.map Name

                    Age i ->
                        Shrink.int i |> Shrink.map Age
        in
            Producer generator shrinker
-}
type alias Producer a =
    { generator : Generator a
    , shrinker : Shrinker a
    }


{-| A producer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Producer ()
unit =
    Producer (Random.Extra.constant ()) Shrink.noShrink


{-| A producer for bool values.
-}
bool : Producer Bool
bool =
    Producer (Random.bool) Shrink.bool


{-| A producer for order values.
-}
order : Producer Order
order =
    Producer (Random.Order.order) Shrink.order


{-| A producer for int values.
-}
int : Producer Int
int =
    let
        generator =
            Random.Extra.frequency
                [ ( 3, Random.int -50 50 )
                , ( 0.2, Random.Extra.constant 0 )
                , ( 1, Random.int 0 (Random.maxInt - Random.minInt) )
                , ( 1, Random.int (Random.minInt - Random.maxInt) 0 )
                ]
    in
        Producer generator Shrink.int


{-| A producer for int values within between a given minimum and maximum value,
inclusive. Shrunken values will also be within the range.
-}
rangeInt : Int -> Int -> Producer Int
rangeInt min max =
    Producer (Random.int min max)
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.int)


{-| A producer for float values. It will never produce `NaN`, `Infinity`, or `-Infinity`.
-}
float : Producer Float
float =
    let
        generator =
            Random.Extra.frequency
                [ ( 3, Random.float -50 50 )
                , ( 0.5, Random.Extra.constant 0 )
                , ( 1, Random.float -1 1 )
                , ( 1, Random.float 0 (toFloat <| Random.maxInt - Random.minInt) )
                , ( 1, Random.float (toFloat <| Random.minInt - Random.maxInt) 0 )
                ]
    in
        Producer generator Shrink.float


{-| A producer for float values within between a given minimum and maximum
value, inclusive. Shrunken values will also be within the range.
-}
rangeFloat : Float -> Float -> Producer Float
rangeFloat min max =
    Producer (Random.float min max)
        (Shrink.keepIf (\i -> i >= min && i <= max) Shrink.float)


{-| A producer for percentage values. Generates random floats between `0.0` and
`1.0`.
-}
percentage : Producer Float
percentage =
    let
        generator =
            Random.Extra.frequency
                [ ( 8, Random.float 0 1 )
                , ( 1, Random.Extra.constant 0 )
                , ( 1, Random.Extra.constant 1 )
                ]
    in
        Producer generator Shrink.float


{-| A producer for ASCII char values.
-}
ascii : Producer Char
ascii =
    Producer Random.Char.ascii Shrink.char


{-| A producer for char values. Generates random ascii chars disregarding the control
characters.
-}
char : Producer Char
char =
    Producer (Random.Char.char 32 126) Shrink.character


{-| A producer for uppercase char values.
-}
upperCaseChar : Producer Char
upperCaseChar =
    Producer Random.Char.upperCaseLatin Shrink.character


{-| A producer for lowercase char values.
-}
lowerCaseChar : Producer Char
lowerCaseChar =
    Producer Random.Char.lowerCaseLatin Shrink.character


{-| A producer for unicode char values.
-}
unicode : Producer Char
unicode =
    Producer Random.Char.unicode Shrink.char


{-| A producer for string values. Generates random printable ascii strings whose
length is between 0 and 10.
-}
string : Producer String
string =
    Producer (Random.String.rangeLengthString 0 10 char.generator)
        Shrink.string


{-| Given a producer of a type, create a producer of a maybe for that type.
-}
maybe : Producer a -> Producer (Maybe a)
maybe prod =
    let
        genBool =
            Random.map not <| Random.Extra.oneIn 4
    in
        Producer (Random.Extra.maybe genBool prod.generator) (Shrink.maybe prod.shrinker)


{-| Given producers for an error type and a success type, createa a producer for
a result.
-}
result : Producer error -> Producer value -> Producer (Result error value)
result errProd valProd =
    Producer (Random.Extra.result Random.bool errProd.generator valProd.generator)
        (Shrink.result errProd.shrinker valProd.shrinker)


{-| Given a producer of a type, create a producer of a list of that type.
Generates random lists of varying length, favoring shorter lists.
-}
list : Producer a -> Producer (List a)
list prod =
    Producer
        (Random.Extra.frequency
            [ ( 1, Random.Extra.constant [] )
            , ( 1, Random.map (\x -> [ x ]) prod.generator )
            , ( 3, Random.Extra.rangeLengthList 2 10 prod.generator )
            , ( 2, Random.Extra.rangeLengthList 10 100 prod.generator )
            , ( 0.5, Random.Extra.rangeLengthList 100 400 prod.generator )
            ]
        )
        (Shrink.list prod.shrinker)


{-| Given a producer of a type, create a producer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Producer a -> Producer (Array a)
array prod =
    Producer
        (Random.Extra.frequency
            [ ( 1, Random.Extra.constant Array.empty )
            , ( 1, Random.map (Array.repeat 1) prod.generator )
            , ( 3, Random.Array.rangeLengthArray 2 10 prod.generator )
            , ( 2, Random.Array.rangeLengthArray 10 100 prod.generator )
            , ( 0.5, Random.Array.rangeLengthArray 100 400 prod.generator )
            ]
        )
        (Shrink.array prod.shrinker)


{-| Turn a tuple of producers into a producer of tuples.
-}
tuple : ( Producer a, Producer b ) -> Producer ( a, b )
tuple ( prodA, prodB ) =
    Producer (Random.map2 (,) prodA.generator prodB.generator)
        (Shrink.tuple ( prodA.shrinker, prodB.shrinker ))


{-| Turn a 3-tuple of producers into a producer of 3-tuples.
-}
tuple3 : ( Producer a, Producer b, Producer c ) -> Producer ( a, b, c )
tuple3 ( prodA, prodB, prodC ) =
    Producer (Random.map3 (,,) prodA.generator prodB.generator prodC.generator)
        (Shrink.tuple3 ( prodA.shrinker, prodB.shrinker, prodC.shrinker ))


{-| Turn a 4-tuple of producers into a producer of 4-tuples.
-}
tuple4 : ( Producer a, Producer b, Producer c, Producer d ) -> Producer ( a, b, c, d )
tuple4 ( prodA, prodB, prodC, prodD ) =
    Producer (Random.map4 (,,,) prodA.generator prodB.generator prodC.generator prodD.generator)
        (Shrink.tuple4 ( prodA.shrinker, prodB.shrinker, prodC.shrinker, prodD.shrinker ))


{-| Turn a 5-tuple of producers into a producer of 5-tuples.
-}
tuple5 : ( Producer a, Producer b, Producer c, Producer d, Producer e ) -> Producer ( a, b, c, d, e )
tuple5 ( prodA, prodB, prodC, prodD, prodE ) =
    Producer (Random.map5 (,,,,) prodA.generator prodB.generator prodC.generator prodD.generator prodE.generator)
        (Shrink.tuple5 ( prodA.shrinker, prodB.shrinker, prodC.shrinker, prodD.shrinker, prodE.shrinker ))


{-| Filter the values from a Producer. The resulting Producer will only generate
random test values or shrunken values that satisfy the predicate. The predicate
must be satisfiable.
-}
filter : (a -> Bool) -> Producer a -> Producer a
filter predicate prod =
    Producer (Random.Extra.filter predicate prod.generator)
        (Shrink.keepIf predicate prod.shrinker)


{-| Convert the output of one producer to another type. This is useful if
you're testing a function that expects a large model record, but you only need
to randomize a few fields. You might do this several different ways for a single
model, so you generate and shrink only the fields relevant to each test.

    type alias Person =
      { first : String, last : String, age : String }

    spy : Producer Person
    spy = convert (\age -> Person "James" "Bond" age) .age (rangeInt 0 120)

In order for shrinking to work, you need to pass an inverse function of the
function being mapped.
-}
convert : (a -> b) -> (b -> a) -> Producer a -> Producer b
convert f g prod =
    Producer (Random.map f prod.generator)
        (Shrink.convert f g prod.shrinker)


{-| Map a function over an producer. This works exactly like `convert`,
except it does not require an inverse function, and consequently does no
shrinking.
-}
map : (a -> b) -> Producer a -> Producer b
map f prod =
    Producer (Random.map f prod.generator)
        Shrink.noShrink
