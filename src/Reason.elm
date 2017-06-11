module Reason exposing (..)

import Ast.Expression exposing (..)
import Regex
import Ast.Statement exposing (..)


-- TODO: chunks the parser barfs ==> feed through directly or as comment


bindingSymbol : Expression -> String
bindingSymbol lhs =
    case lhs of
        Variable _ ->
            " = "

        Application _ _ ->
            " => "

        _ ->
            " = "


showList : (a -> String) -> String -> List a -> String
showList pr sep l =
    case l of
        [] ->
            ""

        [ f ] ->
            pr f

        f :: r ->
            pr f ++ sep ++ showList pr sep r


showPair : String -> (a -> String) -> (a -> String) -> String -> ( a, a ) -> String
showPair prefix prA prSep suffix ( l, r ) =
    showABPair prefix prA prSep prA suffix ( l, r )






showABPair : String -> (a -> String) -> (a -> String) -> (b -> String) -> String -> ( a, b ) -> String
showABPair prefix prA prSep prB suffix ( l, r ) =
    prefix ++ prA l ++ prSep l ++ prB r ++ suffix


simply : String -> a -> String
simply s =
    \_ -> s


errorString e =
    "???" ++ (toString e) ++ "???"


expToReason : Expression -> String
expToReason e =
    case e of
        Character c ->
            "'" ++ (String.fromChar c) ++ "'"

        Integer i ->
            toString i

        Float f ->
            toString f

        String s ->
            "\"" ++ s ++ "\""

        Variable [ v ] ->
            let
                identifier =
                    Regex.regex "[a-zA-Z0-9_]+"

                isIden =
                    Regex.contains identifier v || v == "()"
            in
                if isIden then
                    v
                else
                    "( " ++ v ++ " )"

        Variable _ ->
            errorString e

        Application e1 e2 ->
            (e1 |> expToReason) ++ " " ++ (e2 |> expToReason)

        BinOp (Variable [ op ]) l r ->
            (l |> expToReason) ++ " " ++ op ++ " " ++ (r |> expToReason)

        BinOp _ _ _ ->
            errorString e

        Let bindingPairs inExpr ->
            let
                listOfBindings =
                    showList (showPair "" expToReason bindingSymbol "") " ; " bindingPairs
            in
                "{ let "
                    ++ listOfBindings
                    ++ " ; "
                    ++ (expToReason inExpr)
                    ++ " }"

        Case val casePairList ->
            let
                listOfCases =
                    showList (showPair " | " expToReason (simply " => ") "") "" casePairList
            in
                "switch "
                    ++ (expToReason val)
                    ++ " { "
                    ++ listOfCases
                    ++ " } "

        Tuple exprList ->
            let
                elemList =
                    showList expToReason " , " exprList
            in
                " ( " ++ elemList ++ " ) "

        List exprList ->
            let
                elemList =
                    showList expToReason " , " exprList
            in
                " [ " ++ elemList ++ " ] "

        Record keyValueList ->
            let
                elemList =
                    showList (showABPair "" identity (simply " : ") expToReason "") " , " keyValueList
            in
                "{ " ++ elemList ++ " }"

        RecordUpdate base keyValueList ->
            let
                elemList =
                    showList (showABPair "" identity (simply " : ") expToReason "") " , " keyValueList
            in
                "{ ..." ++ base ++ " , " ++ elemList ++ " }"

        If cond e1 e2 ->
            "if ( "
                ++ (expToReason cond)
                ++ " ) "
                ++ " { "
                ++ (expToReason e1)
                ++ " } "
                ++ " else { "
                ++ (expToReason e2)
                ++ " }"

        Lambda argsList e ->
            let
                args =
                    showList expToReason " " argsList
            in
                "( fun " ++ args ++ " => " ++ (expToReason e) ++ " )"

        Access expr fieldList ->
            let
                path =
                    showList identity " . " fieldList
            in
                (expToReason expr) ++ " . " ++ path

        AccessFunction name ->
            "( fun x => x . " ++ name ++ " )"


statementToReason : Statement -> String
statementToReason stmt =
    case stmt of
    ModuleDeclaration _ _ -> ""
    _ -> "??"