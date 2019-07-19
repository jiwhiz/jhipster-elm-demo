module Utils exposing
    ( delay
    , flip
    , perform
    , split
    , tupleExtend
    , tupleMapThree
    , unzipTripple
    )

import List
import Process
import Task


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b


tupleMapThree : (a -> x) -> (b -> y) -> (c -> z) -> ( a, b, c ) -> ( x, y, z )
tupleMapThree funcA funcB funcC ( x, y, z ) =
    ( funcA x, funcB y, funcC z )


tupleExtend : ( a, b ) -> c -> ( a, b, c )
tupleExtend ( a, b ) c =
    ( a, b, c )


unzipTripple : List ( a, b, c ) -> ( List a, List b, List c )
unzipTripple tripples =
    let
        step ( x, y, z ) ( xs, ys, zs ) =
            ( x :: xs, y :: ys, z :: zs )
    in
    List.foldr step ( [], [], [] ) tripples
