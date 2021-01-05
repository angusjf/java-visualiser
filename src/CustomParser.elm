module CustomParser exposing (..)

import Parser as ElmParser exposing ((|.), (|=))

type alias Parser a = String -> Result String (a, String)

succeed : a -> Parser a
succeed x =
    \str -> Ok (x, str)

ignorer : Parser keep -> Parser ignore -> Parser keep
ignorer keep ignore =
    \str ->
        case keep str of
            Ok (x, more) ->
                case ignore more of
                    Ok (_, end) -> Ok (x, end)
                    Err e -> Err e
            Err e ->
                Err e

keeper : Parser (a -> b) -> Parser a -> Parser b
keeper parserFunc parserArg =
    \str ->
        case parserFunc str of
            Ok (x, more) ->
                case parserArg more of
                    Ok (y, end) ->
                        Ok (x y, end)
                    Err e ->
                        Err e
            Err e ->
                Err e

stringTakeWhile : (Char -> Bool) -> String -> String
stringTakeWhile f s =
    case String.uncons s of
        Nothing -> ""
        Just (x, xs) ->
            if f x then
                String.cons x (stringTakeWhile f xs)
            else
                ""

intParser : Parser Int
intParser =
    oneOf
      [ iikmap negate (symbol "-") (spaces) (unsignedIntParser)
      , iikmap identity (symbol "+") (spaces) (unsignedIntParser)
      , unsignedIntParser
      ]

unsignedIntParser : Parser Int
unsignedIntParser =
    \str ->
        let
            n = stringTakeWhile Char.isDigit str
            after = String.dropLeft (String.length n) str
        in
            case String.toInt n of
                Just i ->
                    Ok (i, after)
                Nothing ->
                    Err str

    {-
    ElmParser.oneOf
        [ ElmParser.succeed negate
          |. ElmParser.symbol "-"
          |= secretIntParser
        , ElmParser.succeed identity
          |. ElmParser.symbol "+"
          |= secretIntParser
        , secretIntParser
        ]


elmToCustom : ElmParser.Parser a -> Parser a
elmToCustom p =
    \str ->
        case ElmParser.run p str of
            Ok a -> Ok a
            Err _ -> Err str

secretIntParser : Parser Int
secretIntParser =
    elmToCustom
        ElmParser.number
            { int = Just identity
            , hex = Just identity
            , octal = Just identity
            , binary = Just identity
            , float = Nothing
            }
    -}


floatParser : Parser Float
floatParser = \str -> Err str --Result.map (\(n, b) -> (toFloat n, b)) (intParser str)

kmap f x = keeper (succeed f) x

imap f x = ignorer (succeed f) x

kimap f a b = ignorer (keeper (succeed f) a) b

ikmap f a b = keeper (ignorer (succeed f) a) b

iikmap f a b c = keeper (ignorer (ignorer (succeed f) a) b) c

kikmap f a b c = keeper (ignorer (keeper (succeed f) a) b) c

ikimap f a b c = ignorer (keeper (ignorer (succeed f) a) b) c

kiikmap f a b c d = keeper (ignorer (ignorer (keeper (succeed f) a) b) c) d

kikikmap f a b c d e =
    keeper (ignorer (keeper (ignorer (keeper (succeed f) a) b) c) d) e

kikikiimap func a b c d e f g =
    let
        i = ignorer
        k = keeper
    in
        i (i (k (i (k (i (k (succeed func) a) b) c) d) e) f) g

iikiimap f a b c d e =
    let
        i = ignorer
        k = keeper
    in
        i ( i ( k ( i ( i (succeed f) a) b) c) d) e

kiiikiimap func a b c d e f g = 
    let
        i = ignorer
        k = keeper
    in
        i (i (k (i (i (i (k (succeed func) a) b) c) d) e) f) g

iikiiiiimap func a b c d e f g h =
    let
        i = ignorer
        k = keeper
    in
        i (i (i (i (i (k (i (i (succeed func) a) b) c) d) e) f) g) h

iiiikiiiiiimap func a b c d e f g h j l m =
    let
        i = ignorer
        k = keeper
    in
        i (i (i (i (i (i (k (i (i (i (i (succeed func) a) b) c) d) e) f) g) h) j) l) m
          
iiiikiiikiimap func a b c d e f g h j l m =
    let
        i = ignorer
        k = keeper
    in
        i (i (k (i (i (i (k (i (i (i (i (succeed func) a) b) c) d) e) f) g) h) j) l) m

kikiiikiiikiimap func a b c d e f g h j l m n o =
    let
        i = ignorer
        k = keeper
    in
        i (i (k (i (i (i (k (i (i (i (k (i (k (succeed func) a) b) c) d) e) f) g) h) j) l) m) n) o

iikiiikiimap func a b c d e f g h j =
    let
        i = ignorer
        k = keeper
    in
        i (i (k (i (i (i (k (i (i (succeed func) a) b) c) d) e) f) g) h) j

iikikiimap func a b c d e f g =
    let
        i = ignorer
        k = keeper
    in
        i (i (k (i (k (i (i (succeed func) a) b) c) d) e) f) g

kiiikikikmap func a b c d e f g h j =
    let
        i = ignorer
        k = keeper in
        k (i (k (i (k (i (i (i (k (succeed func) a) b) c) d) e) f) g) h) j

kikikikikmap func a b c d e f g h j =
    let
        i = ignorer
        k = keeper
    in
        k (i (k (i (k (i (k (i (k (succeed func) a) b) c) d) e) f) g) h) j

kiikkiikmap func a b c d e f g h =
    let
        i = ignorer
        k = keeper
    in
        k (i (i (k (k (i (i (k (succeed func) a) b) c) d) e) f) g) h

kikikiiikmap func a b c d e f g h j =
    let
        i = ignorer
        k = keeper
    in
        k (i (i (i (k (i (k (i (k (succeed func) a) b) c) d) e) f) g) h) j

kikikikmap func a b c d e f g =
    let
        i = ignorer
        k = keeper
    in
        k (i (k (i (k (i (k (succeed func) a) b) c) d) e) f) g

kiimap func a b c =
    let
        i = ignorer
        k = keeper
    in
        i (i (k (succeed func) a) b) c

kikiiikikiimap func a b c d e f g h j l m =
    let
        i = ignorer
        k = keeper
    in
        i (i (k (i (k (i (i (i (k (i (k (succeed func) a) b) c) d) e) f) g) h) j) l) m

kiiiiikikmap func a b c d e f g h j =
    let
        i = ignorer
        k = keeper
    in
        k (i (k (i (i (i (i (i (k (succeed func) a) b) c) d) e) f) g) h) j

kiiikmap func a b c d e =
    let
        i = ignorer
        k = keeper
    in
        k (i (i (i (k (succeed func) a) b) c) d) e

iiikiiikmap func a b c d e f g h =
    let
        i = ignorer
        k = keeper
    in
        k (i (i (i (k (i (i (i (succeed func) a) b) c) d) e) f) g) h

iiiikiiikmap func a b c d e f g h j =
    let
        i = ignorer
        k = keeper
    in
        k (i (i (i (k (i (i (i (i (succeed func) a) b) c) d) e) f) g) h) j

kiikikmap func a b c d e f =
    let
        i = ignorer
        k = keeper
    in
        k (i (k (i (i (k (succeed func) a) b) c) d) e) f

ikkmap func a b c = 
    let
        i = ignorer
        k = keeper
    in
        k (k (i (succeed func) a) b) c

iikkmap func a b c d =
    let
        i = ignorer
        k = keeper
    in
        k (k (i (i (succeed func) a) b) c) d

kiiikikmap func a b c d e f g =
    let
        i = ignorer
        k = keeper
    in
        k (i (k (i (i (i (k (succeed func) a) b) c) d) e) f) g

iikikikmap func a b c d e f g =
    let
        i = ignorer
        k = keeper
    in
        k (i (k (i (k (i (i (succeed func) a) b) c) d) e) f) g

iikikmap func a b c d e =
    let
        i = ignorer
        k = keeper
    in
        k (i (k (i (i (succeed func) a) b) c) d) e

kiiiimap func a b c d e =
    let
        i = ignorer
        k = keeper
    in
        i (i (i (i (k (succeed func) a) b) c) d) e

iiiikmap func a b c d e =
    let
        i = ignorer
        k = keeper
    in
        k (i (i (i (i (succeed func) a) b) c) d) e

kiiikiiikmap func a b c d e f g h j =
    let
        i = ignorer
        k = keeper
    in
        k (i (i (i (k (i (i (i (k (succeed func) a) b) c) d) e) f) g) h) j

oneOf : List (Parser a) -> Parser a
oneOf parsers =
    case parsers of
        p::ps ->
            \str ->
                case p str of
                    Ok (x, more) ->
                        Ok (x, more)
                    Err _ ->
                        (oneOf ps) str
        [] ->
            \str -> Err str

keyword : String -> Parser ()
keyword kwd =
    \str ->
        if String.startsWith kwd str then
            Ok ((), String.dropLeft (String.length kwd) str)
        else
            Err str

eof : Parser ()
eof =
    \str ->
        if str == "" then
            Ok ((), "")
        else
            Err str

lazy : (() -> Parser a) -> Parser a
lazy x = \str -> (x ()) str


symbol : String -> Parser ()
symbol kwd = keyword kwd

spaces : Parser ()
spaces =
    \str ->
        Ok ((), String.trimLeft str)


fail : Parser a
fail = \str -> Err str



optional : Parser a -> Parser (Maybe a)
optional p =
    oneOf
        [ kmap Just p
        , succeed Nothing
        ]


nonEmptySep : String -> Parser a -> Parser (List a)
nonEmptySep sep p =
    kikmap (::)
        p
        spaces
        (list
            (iikmap identity
                (symbol sep)
                spaces
                p
            )
        )


list : Parser a -> Parser (List a)
list p =
    \str ->
        case (ignorer p spaces) str of
            Ok (x, more) ->
                case (list p) more of
                    Ok (xs, end) -> Ok (x :: xs, end)
                    Err _ -> Debug.todo "list can't fail?"
            Err _ -> 
                Ok ([], str)


dotted : Parser a -> Parser (List a)
dotted =
    nonEmptySep "."


brackets : Parser Int
brackets =
    let
      bs = list b
      b = 
        ignorer (
        ignorer (
        (symbol "[") ) <|
        spaces ) <|
        (symbol "]")
    in
      \str ->
        case bs str of
          Ok (x, rest) -> Ok (List.length x, rest)
          Err x -> Err x

