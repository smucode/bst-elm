module Tests exposing (..)

import Test exposing (..)
import Expect
import List
import Fuzz exposing (list, int, tuple, string)
import String
import BstSet exposing (..)


all : Test
all =
    describe "BST Set"
        [ describe "constructor"
            [ test "Should create empty set" <|
                \() -> Expect.equal empty Empty
            , test "Should create a set with a single item" <|
                \() -> Expect.equal (singleton 1) (Tree 1 Empty Empty)
            ]
        , describe "insert"
            [ test "Should create set with single item" <|
                \() -> Expect.equal (insert 1 Empty) (Tree 1 Empty Empty)
            ]
        , describe "fromList"
            [ test "Should convert list to set" <|
                \() -> Expect.equal (fromList (List.singleton 1)) (Tree 1 Empty Empty)
            ]
        , describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "Fuzz test examples, using randomly generated input"
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
