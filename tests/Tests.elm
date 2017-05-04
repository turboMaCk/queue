module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String


-- libraris

import Queue exposing (..)
import State


all : Test
all =
    describe "Queue"
        [ describe "constructors"
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
                        Expect.equal (size <| fromList list) <| List.length list
                ]
            , describe "enqueue"
                [ test "enqueue to empty" <|
                    \() ->
                        Expect.equal (empty |> enqueue 1) ( Nothing, fromList [ 1 ] )
                , test "equeue not empty" <|
                    \() ->
                        Expect.equal (fromList [ "foo" ] |> enqueue "bar" |> Tuple.second |> toList) [ "foo", "bar" ]
                ]
            , describe "dequeue"
                [ test "empty" <|
                    \() ->
                        Expect.equal (dequeue empty) ( Nothing, empty )
                , test "not empty" <|
                    \() ->
                        Expect.equal (fromList [ 1 ] |> dequeue) ( Just 1, empty )
                ]
            ]
        ]
