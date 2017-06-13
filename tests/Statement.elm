module Statement exposing (..)

import Ast exposing (parseStatement, parse)
import Ast.BinOp exposing (Assoc(..), operators)
import Ast.Expression exposing (Expression(..))
import Ast.Statement exposing (ExportSet(..), Type(..), Statement(..))
import Expect exposing (..)
import Test exposing (describe, test, Test)
import Reason exposing (..)
import String.Extra


becomes : String -> String -> Expectation
becomes reasonString elmString =
    case parseStatement operators elmString of
        Ok ( _, _, ast ) ->
            Expect.equal reasonString (ast |> statementToReason |> String.Extra.clean)

        _ ->
            Expect.fail ("failed to parse: \"" ++ elmString)


-- are : String -> String -> Expectation
-- are reasonString elmStmtList =
--     case parse elmStmtList of
--         Ok ( _, _, r ) ->
--             Expect.equal r s

--         _ ->
--             Expect.fail ("failed to parse: \"" ++ i ++ "\" <vs> " ++ toString s)


-- moduleDeclaration : Test
-- moduleDeclaration =
--     describe "Module declaration statements"
--         [ test "simple declaration exposing all" <|
--             \() -> "module A exposing (..)" |> becomes "??"
--         , test "declaration exposing particular things" <|
--             \() ->
--                 "module A exposing (A, b)"
--                     |> becomes "??"
--         , test "declaration exposing an infix operator" <|
--             \() ->
--                 "module A exposing ((?))"
--                     |> is
--                         (ModuleDeclaration [ "A" ] <|
--                             SubsetExport [ FunctionExport "?" ]
--                         )
--         , test "declaration exposing union" <|
--             \() ->
--                 "module A exposing (A(..))"
--                     |> is
--                         (ModuleDeclaration [ "A" ] <|
--                             SubsetExport [ TypeExport "A" (Just AllExport) ]
--                         )
--         , test "declaration exposing constructor subset" <|
--             \() ->
--                 "module A exposing (A(A))"
--                     |> is
--                         (ModuleDeclaration [ "A" ] <|
--                             SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ]
--                         )
--         , test "multiline declaration" <|
--             \() ->
--                 "module A exposing (A, B,\nc)"
--                     |> is
--                         (ModuleDeclaration [ "A" ] <|
--                             SubsetExport
--                                 [ TypeExport "A" Nothing
--                                 , TypeExport "B" Nothing
--                                 , FunctionExport "c"
--                                 ]
--                         )
--         , test "declaration using a port" <|
--             \() ->
--                 "port module A exposing (A(..))"
--                     |> is
--                         (PortModuleDeclaration [ "A" ] <|
--                             SubsetExport [ TypeExport "A" (Just AllExport) ]
--                         )
--         , test "simple effects" <|
--             \() ->
--                 "effect module A where {subscription = MySub, command = MyCmd} exposing (..)"
--                     |> is
--                         (EffectModuleDeclaration [ "A" ]
--                             [ ( "subscription", "MySub" )
--                             , ( "command", "MyCmd" )
--                             ]
--                             AllExport
--                         )
        -- ]


importStatements : Test
importStatements =
    describe "Import statements"
        [ test "simple import" <|
            \() -> "import A" |> becomes ""
        , test "import as" <|
            \() -> "import A as B" |> becomes "module B = A ;"
        , test "import exposing all" <|
            \() ->
                "import A exposing (..)"
                    |> becomes "open A ;"
        , test "import exposing" <|
            \() ->
                "import A exposing (A, b)"
                    |> becomes "type A = A . A ; let b = A . b ;"
        , test "import exposing union" <|
            \() ->
                "import A exposing (B(..))"
                    |> becomes "??"
--         , test "import exposing constructor subset" <|
--             \() ->
--                 "import A exposing (A(A))"
--                     |> is
--                         (ImportStatement [ "A" ] Nothing <|
--                             Just <|
--                                 SubsetExport [ TypeExport "A" (Just <| SubsetExport [ FunctionExport "A" ]) ]
--                         )
        , test "import multiline" <|
            \() ->
                "import A as B exposing (A, B,\nc)"
                    |> becomes "module B = A ; type A = A . A ; type B = A . B ; let c = A . c ;"
        ]


-- typeAnnotations : Test
-- typeAnnotations =
--     describe "Type annotations"
--         [ test "constant" <|
--             \() ->
--                 "x : Int"
--                     |> becomes (FunctionTypeDeclaration "x" (TypeConstructor [ "Int" ] []))
--         , test "variables" <|
--             \() ->
--                 "x : a"
--                     |> becomes (FunctionTypeDeclaration "x" (TypeVariable "a"))
--         , test "variables with numbers" <|
--             \() ->
--                 "x : a1"
--                     |> becomes (FunctionTypeDeclaration "x" (TypeVariable "a1"))
--         , test "application" <|
--             \() ->
--                 "x : a -> b"
--                     |> is
--                         (FunctionTypeDeclaration "x"
--                             (TypeApplication
--                                 (TypeVariable "a")
--                                 (TypeVariable "b")
--                             )
--                         )
--         , test "application associativity" <|
--             \() ->
--                 "x : a -> b -> c"
--                     |> is
--                         (FunctionTypeDeclaration "x"
--                             (TypeApplication
--                                 (TypeVariable "a")
--                                 (TypeApplication
--                                     (TypeVariable "b")
--                                     (TypeVariable "c")
--                                 )
--                             )
--                         )
--         , test "application parens" <|
--             \() ->
--                 "x : (a -> b) -> c"
--                     |> is
--                         (FunctionTypeDeclaration "x"
--                             (TypeApplication
--                                 (TypeApplication
--                                     (TypeVariable "a")
--                                     (TypeVariable "b")
--                                 )
--                                 (TypeVariable "c")
--                             )
--                         )
--         , test "qualified types" <|
--             \() ->
--                 "m : Html.App Msg"
--                     |> is
--                         (FunctionTypeDeclaration "m"
--                             (TypeConstructor
--                                 [ "Html", "App" ]
--                                 [ (TypeConstructor [ "Msg" ] []) ]
--                             )
--                         )
--         ]


-- portStatements : Test
-- portStatements =
--     describe "port type declaration"
--         [ test "constant" <|
--             \() ->
--                 "port focus : String -> Cmd msg"
--                     |> is
--                         (PortTypeDeclaration "focus"
--                             (TypeApplication
--                                 (TypeConstructor [ "String" ] [])
--                                 (TypeConstructor [ "Cmd" ]
--                                     ([ TypeVariable "msg" ])
--                                 )
--                             )
--                         )
--         , test "another port type declaration" <|
--             \() ->
--                 "port users : (User -> msg) -> Sub msg"
--                     |> is
--                         (PortTypeDeclaration "users"
--                             (TypeApplication
--                                 (TypeApplication
--                                     (TypeConstructor [ "User" ] [])
--                                     (TypeVariable "msg")
--                                 )
--                                 (TypeConstructor [ "Sub" ]
--                                     ([ TypeVariable "msg" ])
--                                 )
--                             )
--                         )
--         , test "port definition" <|
--             \() ->
--                 "port focus = Cmd.none"
--                     |> is
--                         (PortDeclaration "focus"
--                             []
--                             (Access
--                                 (Variable [ "Cmd" ])
--                                 [ "none" ]
--                             )
--                         )
--         ]


-- infixDeclarations : Test
-- infixDeclarations =
--     describe "Infix declarations"
--         [ test "non" <|
--             \() ->
--                 "infix 9 :-"
--                     |> becomes (InfixDeclaration N 9 ":-")
--         , test "left" <|
--             \() ->
--                 "infixl 9 :-"
--                     |> becomes (InfixDeclaration L 9 ":-")
--         , test "right" <|
--             \() ->
--                 "infixr 9 :-"
--                     |> becomes (InfixDeclaration R 9 ":-")
--         ]


-- singleDeclarationInput : String
-- singleDeclarationInput =
--     """
-- f : Int -> Int
-- f x =
--   x + 1
-- """


-- singleDeclaration : Test
-- singleDeclaration =
--     test "simple function" <|
--         \() ->
--             singleDeclarationInput
--                 |> are
--                     [ FunctionTypeDeclaration "f"
--                         (TypeApplication
--                             (TypeConstructor [ "Int" ] [])
--                             (TypeConstructor [ "Int" ] [])
--                         )
--                     , FunctionDeclaration "f"
--                         [ Variable [ "x" ] ]
--                         (BinOp
--                             (Variable [ "+" ])
--                             (Variable [ "x" ])
--                             (Integer 1)
--                         )
--                     ]


-- multipleDeclarationsInput : String
-- multipleDeclarationsInput =
--     """


-- f : Int -> Int
-- f x =
--   x + 1

-- g : Int -> Int
-- g x =
--   f x + 1

-- h : (Int, Int) -> Int
-- h (a, b) = a + b

-- (+) : Int -> Int
-- (+) a b =
--   1
-- """


-- multipleDeclarations : Test
-- multipleDeclarations =
--     test "multiple functions" <|
--         \() ->
--             multipleDeclarationsInput
--                 |> are
--                     [ FunctionTypeDeclaration "f"
--                         (TypeApplication
--                             (TypeConstructor [ "Int" ] [])
--                             (TypeConstructor [ "Int" ] [])
--                         )
--                     , FunctionDeclaration "f"
--                         [ Variable [ "x" ] ]
--                         (BinOp
--                             (Variable [ "+" ])
--                             (Variable [ "x" ])
--                             (Integer 1)
--                         )
--                     , FunctionTypeDeclaration "g"
--                         (TypeApplication
--                             (TypeConstructor [ "Int" ] [])
--                             (TypeConstructor [ "Int" ] [])
--                         )
--                     , FunctionDeclaration "g"
--                         [ Variable [ "x" ] ]
--                         (BinOp
--                             (Variable [ "+" ])
--                             (Application
--                                 (Variable [ "f" ])
--                                 (Variable [ "x" ])
--                             )
--                             (Integer 1)
--                         )
--                     , FunctionTypeDeclaration "h"
--                         (TypeApplication
--                             ((TypeTuple ([ TypeConstructor [ "Int" ] [], TypeConstructor [ "Int" ] [] ])))
--                             (TypeConstructor [ "Int" ] [])
--                         )
--                     , FunctionDeclaration "h"
--                         [ Tuple [ (Variable [ "a" ]), (Variable [ "b" ]) ] ]
--                         (BinOp
--                             (Variable [ "+" ])
--                             (Variable [ "a" ])
--                             (Variable [ "b" ])
--                         )
--                     , FunctionTypeDeclaration "+"
--                         (TypeApplication
--                             (TypeConstructor [ "Int" ] [])
--                             (TypeConstructor [ "Int" ] [])
--                         )
--                     , FunctionDeclaration "+" [ Variable [ "a" ], Variable [ "b" ] ] (Integer 1)
--                     ]


-- moduleFixityInput : String
-- moduleFixityInput =
--     """
-- f = a ++ b ++ c

-- infixl 1 ++

-- g = a ** b ** c

-- infixr 1 **
-- """


-- moduleFixityDeclarations : Test
-- moduleFixityDeclarations =
--     test "module fixity scanning" <|
--         \() ->
--             moduleFixityInput
--                 |> are
--                     [ FunctionDeclaration "f"
--                         []
--                         (BinOp
--                             (Variable [ "++" ])
--                             (BinOp
--                                 (Variable [ "++" ])
--                                 (Variable [ "a" ])
--                                 (Variable [ "b" ])
--                             )
--                             (Variable [ "c" ])
--                         )
--                     , InfixDeclaration L 1 "++"
--                     , FunctionDeclaration "g"
--                         []
--                         (BinOp
--                             (Variable [ "**" ])
--                             (Variable [ "a" ])
--                             (BinOp
--                                 (Variable [ "**" ])
--                                 (Variable [ "b" ])
--                                 (Variable [ "c" ])
--                             )
--                         )
--                     , InfixDeclaration R 1 "**"
--                     ]


-- emptyRecordAliasInput : String
-- emptyRecordAliasInput =
--     """
-- type alias A = {}
-- """


-- emptyTupleAliasInput : String
-- emptyTupleAliasInput =
--     """
-- type alias A = ()
-- """


-- typeDeclarations : Test
-- typeDeclarations =
--     describe "type declarations"
--         [ test "can parse empty record aliases" <|
--             \() ->
--                 emptyRecordAliasInput |> are [ TypeAliasDeclaration (TypeConstructor [ "A" ] []) (TypeRecord []) ]
--         , test "can parse aliases of unit" <|
--             \() ->
--                 emptyTupleAliasInput |> are [ TypeAliasDeclaration (TypeConstructor [ "A" ] []) (TypeTuple []) ]
--         ]

