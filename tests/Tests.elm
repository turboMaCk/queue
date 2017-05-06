module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


-- libraries

import Queue exposing (..)
import State exposing (State)


computation : Queue Int -> ( Maybe Int, Queue Int )
computation initial =
    let
        square : Maybe Int -> State (Queue Int) (Maybe Int)
        square maybeInt =
            case maybeInt of
                Just int ->
                    State.advance <| (,) Nothing << (enqueue <| int ^ 2)

                Nothing ->
                    State.advance <| (,) Nothing

        add : Int -> Maybe Int -> State (Queue Int) (Maybe Int)
        add num _ =
            State.advance <| (,) Nothing << (enqueue num)

        getState : Maybe Int -> State (Queue Int) (Maybe Int)
        getState _ =
            State.advance dequeue
    in
        State.embed next
            |> State.andThen (add 10)
            -- add 10 to queue
            |>
                State.andThen (add 20)
            -- add 20 to queue
            |>
                State.andThen getState
            -- this reads first value in queue
            |>
                State.andThen square
            -- square readed value and enqueue it (end)
            |>
                State.andThen getState
            -- read (new) first value in queue
            |>
                State.andThen square
            -- square (new) value and enqueue it (at the end)
            |>
                State.andThen getState
            -- get next Value in queue
            |>
                State.run initial


monadic : Test
monadic =
    let
        withList : List Int -> ( Maybe Int, List Int )
        withList =
            (\( value, queue ) ->
                ( value, toList queue )
            )
                << computation
                << fromList
    in
        describe "monadic computation"
            [ test "with []" <|
                \() ->
                    Expect.equal (withList []) ( Just 100, [ 400 ] )
            , test "with [ 8 ]" <|
                \() ->
                    Expect.equal (withList [ 8 ]) ( Just 20, [ 64, 100 ] )
            , test "with [ 1, 2 ]" <|
                \() ->
                    Expect.equal (withList [ 1, 2 ]) ( Just 10, [ 20, 1, 4 ] )
            ]


enqueueList : List a -> Queue a -> Queue a
enqueueList list queue =
    case list of
        [] ->
            queue

        head :: tail ->
            let
                add : Maybe a -> State (Queue a) (Maybe a)
                add maybeThing =
                    case maybeThing of
                        Just thing ->
                            State.advance <| (,) Nothing << (enqueue thing)

                        Nothing ->
                            State.advance <| (,) Nothing
            in
                State.state (Just head)
                    |> State.andThen add
                    |> State.run queue
                    |> Tuple.second
                    |> enqueueList tail


usefulMonadic : Test
usefulMonadic =
    describe "Useful monadic"
        [ describe "equeeu list as separate items"
            [ fuzz (list string) "empty queue" <|
                \list ->
                    Expect.equal (toList <| enqueueList list empty) list
            , fuzz (list string) "nonempty queue" <|
                \list ->
                    Expect.equal (toList <| enqueueList list <| fromList [ "foo", "bar" ]) <| [ "foo", "bar" ] ++ list
            ]
        ]


all : Test
all =
    describe "Queue"
        [ describe "empty"
            [ test "empty" <|
                \() ->
                    Expect.equal (toList empty) []
            ]
        , describe "tolist & fromList"
            [ test "some" <|
                \() ->
                    Expect.equal (toList <| fromList [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , fuzz (list string) "fuzzy" <|
                \list ->
                    Expect.equal (toList <| fromList list) list
            ]
        , describe "isEmpty"
            [ test "empty" <|
                \() ->
                    Expect.true "empty" <| isEmpty empty
            , test "not empty" <|
                \() ->
                    Expect.false "not empty" <| isEmpty <| fromList [ 1 ]
            ]
        , describe "size"
            [ fuzz (list string) "fuzzy" <|
                \list ->
                    Expect.equal (size <| enqueue "bar" <| fromList list) ((+) 1 <| List.length list)
            ]
        , describe "enqueue"
            [ test "enqueue to empty" <|
                \() ->
                    Expect.equal (empty |> enqueue 1) <| fromList [ 1 ]
            , test "equeue not empty" <|
                \() ->
                    Expect.equal (fromList [ "foo" ] |> enqueue "bar" |> toList) [ "foo", "bar" ]
            ]
        , describe "dequeue"
            [ test "empty" <|
                \() ->
                    Expect.equal (dequeue empty) ( Nothing, empty )
            , test "not empty" <|
                \() ->
                    Expect.equal (dequeue <| fromList [ 1 ]) ( Just 1, empty )
            ]
        , describe "next"
            [ test "empty" <|
                \() ->
                    Expect.equal (next empty) Nothing
            , test "not empty" <|
                \() ->
                    Expect.equal (next <| fromList [ 1 ]) <| Just 1
            ]
        , describe "map"
            [ fuzz (list string) "identity" <|
                \list ->
                    Expect.equal (toList <| map identity <| fromList list) list
            , test "addition" <|
                \list ->
                    Expect.equal (toList <| map ((+) 1) <| fromList [ 1, 2, 3 ]) [ 2, 3, 4 ]
            ]
        , describe "filter"
            [ test "list" <|
                \() ->
                    Expect.equal (toList <| Queue.filter ((<) 2) <| fromList [ 1, 2, 3 ]) [ 3 ]
            ]
        , monadic
        , usefulMonadic
        ]
