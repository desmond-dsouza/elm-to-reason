module Expression exposing (..)

import Ast exposing (parseExpression, parseStatement)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)
import Expect exposing (..)
import String
import Test exposing (describe, test, Test)
import String.Extra

import Reason exposing (..)

strip : Test
strip =
    test "strips all whitespace" <|
        \() -> String.Extra.clean "  a\t\nb  c" |> Expect.equal "a b c"


becomes : String -> String -> Expectation
becomes reasonString elmString =
    case parseExpression operators (String.trim elmString) of
        Ok ( _, _, ast ) ->
            let stringClean = case ast of
                Character _ -> identity
                String _ -> identity
                _ -> String.Extra.clean 
            in
                Expect.equal reasonString (ast |> expToReason |> stringClean) 

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ elmString ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


-- fails : String -> Expectation
-- fails s =
--     case parseExpression operators s of
--         Err _ ->
--             Expect.pass

--         _ ->
--             Expect.fail (s ++ " expected to fail")


characterLiterals : Test
characterLiterals =
    describe "Character literals"
        [ test "character literal" <|
            \() -> "'a'" |> becomes "'a'"
        , test "newline literal" <|
            \() -> "'\n'" |> becomes "'\n'"
        ]


intLiterals : Test
intLiterals =
    describe "Integer literals"
        [ test "integer literal" <|
            \() -> "0" |> becomes "0"
        , test "positive literal" <|
            \() -> "+12" |> becomes "12"
        , test "negative literal" <|
            \() -> "-12" |> becomes "-12"
        ]


floatLiterals : Test
floatLiterals =
    describe "Float literals"
        [ test "float literal" <|
            \() -> "0.5" |> becomes "0.5"
        , test "positive literal" <|
            \() -> "+12.5" |> becomes "12.5"
        , test "negative literal" <|
            \() -> "-12.5" |> becomes "-12.5"
        ]


stringLiterals : Test
stringLiterals =
    describe "String literals"
        [ test "empty string" <|
            \() -> "\"\"" |> becomes "\"\""
        , test "simple string" <|
            \() -> "\"hello\"" |> becomes "\"hello\""
        , test "escaped string" <|
            \() -> "\"hello, \\\"world\\\"\"" |> becomes "\"hello, \\\"world\\\"\""
        , test "triple-quoted string" <|
            \() -> "\"\"\"\"\"\"" |> becomes "\"\""
        , test "multi-line strings" <|
            \() -> "\"\"\"hello\nworld\"\"\"" |> becomes "\"hello\nworld\""
        , test "double escaped string" <|
            \() -> "\"\\\\\"" |> becomes "\"\\\\\""
        ]

letExpressions : Test
letExpressions =
    describe "Let"
        [ test "single binding" <|
            \() ->
                "let a = 42 in a"
                    |> becomes "{ let a = 42 ; a }"
        , test "bind to _" <|
            \() ->
                "let _ = 42 in 24"
                    |> becomes "{ let _ = 42 ; 24 }"
        , test "function1" <|
            \() ->
                """
let
  f x = x + 1
in
  f 4
        """
                    |> becomes "{ let f x => x + 1 ; f 4 }"
        , test "function2" <|
            \() ->
                """
let
  f x = x + 1
  g x = x + 1
in
  f 4
        """
                    |> becomes "{ let f x => x + 1 ; g x => x + 1 ; f 4 }"
        , test "multiple bindings" <|
            \() ->
                """
let
  a = 42

  b = a + 1
in
  b
            """
                    |> becomes "{ let a = 42 ; b = a + 1 ; b }"

        ]


caseExpressions : Test
caseExpressions =
    describe "Case"
        [ test "simple statement" <|
            \() ->
                """
case x of
  Nothing ->
    0

  Just y ->
    y
          """
                    |> becomes "switch x { | Nothing => 0 | Just y => y }"

        , test "binding to underscore" <|
            \() ->
                """
case x of
  _ ->
    42
          """
                    |> becomes "switch x { | _ => 42 }"
        ]


application : Test
application =
    describe "Application"
        [ test "simple application" <|
            \() ->
                "f a"
                    |> becomes "f a"
        , test "curried application" <|
            \() ->
                "f a b"
                    |> becomes "f a b"
        , test "curried application with parens" <|
            \() ->
                "(f a) b"
                    |> becomes "f a b"

        , test "multiline application" <|
            \() ->
                "f\n a\n b"
                    |> becomes "f a b"
        , test "multiline bug" <|
            \() ->
                "f\n (==)"
                    |> becomes "f ( == )"

        , test "same multiline bug" <|
            \() ->
                "f\n \"I like the symbol =\""
                    |> becomes "f \"I like the symbol =\""
        , test "constructor application" <|
            \() ->
                "Cons a Nil"
                    |> becomes "Cons a Nil"
        ]


tuple : Test
tuple =
    describe "Tuples"
        [ test "Empty tuple" <|
            \() -> "()" |> becomes "()"
        , test "Simple tuple" <|
            \() ->
                "(a, b)"
                    |> becomes "( a , b )"
        , test "Simple tuple with format" <|
            \() ->
                "( a, b )"
                    |> becomes "( a , b )"
        ]


list : Test
list =
    describe "Lists"
        [ test "Empty list" <| \() -> "[]" |> becomes "[ ]"
        , test "Simple list" <|
            \() -> "[1, 2]" |> becomes "[ 1 , 2 ]"
        , test "List of tuples" <|
            \() ->
                "[(a, b), (a, b)]"
                    |> becomes "[ ( a , b ) , ( a , b ) ]"
        ]


record : Test
record =
    describe "Records"
        [ test "Simple record" <|
            \() ->
                "{a = b}"
                    |> becomes "{ a : b }"
        , test "Simple record with many fields" <|
            \() ->
                "{a = b, b = 2}"
                    |> becomes "{ a : b , b : 2 }"
        , test "Simple record with many tuple fields" <|
            \() ->
                "{a = (a, b), b = (a, b)}"
                    |> becomes "{ a : ( a , b ) , b : ( a , b ) }"
        , test "Simple record with updated field" <|
            \() ->
                "{a | b = 2, c = 3}"
                    |> becomes "{ ...a , b : 2 , c : 3 }"
        , test "Simple record with advanced field" <|
            \() ->
                "{a = Just 2}"
                    |> becomes "{ a : Just 2 }"
        , test "Simple update record with advanced field" <|
            \() ->
                "{a | a = Just 2}"
                    |> becomes "{ ...a , a : Just 2 }"
        , test "Simplified record destructuring pattern" <|
            \() ->
                "{a, b}"
                    |> becomes "{ a : a , b : b }"
        ]

ifElse : Test
ifElse =
    describe "ifElse"
        [ test "basic" <|
            \() ->
                "if c then t else f"
                    |> becomes "if ( c ) { t } else { f }"
        ]


expressions : Test
expressions =
    describe "Expressions"
        [ test "Operator in parens" <|
            \() -> "(+)" |> becomes "( + )"
        , test "Operators passed to map" <|
            \() ->
                "reduce (+) list"
                    |> becomes "reduce ( + ) list"
        , test "partial application" <|
            \() -> "(+) 2" |> becomes "( + ) 2"
        , test "Case with as" <|
            \() ->
                "case a of \nT _ as x -> 1"
                    |> becomes "switch a { | T _ as x => 1 }"
        , test "cons has right assoc" <|
            \() ->
                "a :: b :: c"
                    |> becomes "a :: b :: c"
        , test "cons has right assoc with tuple" <|
            \() ->
                "(a, a :: b :: c)"
                    |> becomes "( a , a :: b :: c )"
        , test "Destructuring lambda" <|
            \() ->
                "\\(a,b) acc -> 1"
                    |> becomes "( fun ( a , b ) acc => 1 )"
        , test "Destructuring Let" <|
            \() ->
                "let (a,b) = (1,2) in a"
                    |> becomes "{ let ( a , b ) = ( 1 , 2 ) ; a }"
        , test "Access" <|
            \() ->
                "Module.a"
                    |> becomes "Module . a"
        , test "AccessFunction" <|
            \() ->
                "map .a list"
                    |> becomes "map ( fun x => x . a ) list"
        ]

