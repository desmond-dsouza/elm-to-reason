module Reason exposing (..)

import Ast.Expression exposing (..)
import Regex
import Ast.Statement exposing (..)
import String.Extra


-- TODO: chunks the parser barfs ==> feed through directly or as comment


notDone a =
    "/* ?? " ++ (toString a) ++ " ?? */"


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

        Variable (h :: t) ->
            let
                identifier =
                    Regex.regex "[a-zA-Z0-9_]+"

                isIden =
                    Regex.contains identifier h || h == "()"
            in
                if isIden then
                    showList identity " . " (h :: t)
                else
                    "( " ++ h ++ " )"

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


typeNameToReason : String -> String
typeNameToReason s =
    String.Extra.replaceSlice (String.toLower (String.slice 0 1 s)) 0 1 s


dottedName : List String -> String
dottedName l =
    showList identity " . " l


typeVarToReason : Type -> String
typeVarToReason t =
    case t of
        TypeVariable s ->
            "'" ++ s

        _ ->
            notDone (toString t)


todo : String -> String
todo s =
    " /* TODO: " ++ s ++ " */"


statementToReason : Statement -> String
statementToReason stmt =
    case stmt of
        ModuleDeclaration name exports ->
            notDone (toString stmt)

        ImportStatement dottedList Nothing Nothing ->
            ""

        ImportStatement dottedList renamed exposeList ->
            let
                name =
                    dottedName dottedList

                localModule =
                    case renamed of
                        Just n ->
                            "module " ++ n ++ " = " ++ name ++ " ; "

                        Nothing ->
                            ""

                exportSet e =
                    case e of
                        AllExport ->
                            "open " ++ name ++ " ; "

                        SubsetExport l ->
                            List.map exportSet l |> List.foldr (\a b -> a ++ b) ""
                            
                        FunctionExport n ->
                            "let " ++ n ++ " = " ++ name ++ " . " ++ n ++ " ; "

                        TypeExport t es ->
                            let
                                exportedConstructors =
                                    case es of
                                        Just AllExport ->
                                            todo ("expose '" ++ t ++ "' constructors")

                                        Just (SubsetExport l) ->
                                            todo (exportSet (SubsetExport l))

                                        _ ->
                                            ""
                                tR = typeNameToReason t
                            in
                                "type " ++ tR ++ " = " ++ (dottedName (List.append dottedList [ tR ])) ++ " ; " ++ exportedConstructors

                localNames =
                    case exposeList of
                        Just es ->
                            exportSet es

                        Nothing ->
                            ""
            in
                localModule ++ localNames

        TypeAliasDeclaration (TypeConstructor [ name ] vars) t2 ->
            let
                varList =
                    showList typeVarToReason " " vars
            in
                "type " ++ (typeNameToReason name) ++ varList ++ " = " ++ toString t2

        FunctionTypeDeclaration name t -> 
            notDone ("type decl." ++ name)
        _ ->
            notDone (toString stmt)
