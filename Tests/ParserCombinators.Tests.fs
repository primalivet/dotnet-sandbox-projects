module Sandbox.ParserCombinatorsTests

open System
open NUnit.Framework
open FsUnit
open Sandbox.ParserCombinators

[<Test>]
let ``pchar success case`` () =
    "hello world" |> run (pchar 'h') |> should equal (Success('h', "ello world"))

[<Test>]
let ``pchar failure case`` () =
    "ello world"
    |> run (pchar 'h')
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'h'. Got: 'e'\""

[<Test>]
let ``pchar empty input case`` () =
    ""
    |> run (pchar 'h')
    |> (fun x -> x.ToString())
    |> should equal "Failure \"No more input\""

[<Test>]
let ``.>>. (andThen) combines two parsers`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'e' in
    let parseHThenE = parseH .>>. parseE

    "hello world"
    |> run parseHThenE
    |> should equal (Success(('h', 'e'), "llo world"))

[<Test>]
let ``.>>. (andThen) fails on first parser`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'e' in
    let parseHThenE = parseH .>>. parseE

    "jello world"
    |> run parseHThenE
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'h'. Got: 'j'\""

[<Test>]
let ``.>>. (andThen) fails on second parser`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'e' in
    let parseHThenE = parseH .>>. parseE

    "hällo world"
    |> run parseHThenE
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'e'. Got: 'ä'\""

[<Test>]
let ``<|> (orElse) succeed on first parser`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'j' in
    let parseHThenE = parseH <|> parseE

    "hello world" |> run parseHThenE |> should equal (Success(('h'), "ello world"))

[<Test>]
let ``<|> (orElse) succeed on second parser`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'j' in
    let parseHThenE = parseH <|> parseE

    "jello world" |> run parseHThenE |> should equal (Success(('j'), "ello world"))

[<Test>]
let ``<|> (orElse) fail on both parsers`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'j' in
    let parseHThenE = parseH <|> parseE

    "tello world"
    |> run parseHThenE
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'j'. Got: 't'\"" // TODO: should say "Expeted h or j. Got: t"

[<Test>]
let ``Combine .>>. and <|> succeeds to parse "hej"`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'e' in
    let parseJ = pchar 'j' in
    let parseY = pchar 'y' in
    let parseHejOrHey = (parseH .>>. parseE) .>>. (parseJ <|> parseY) in
    "hej" |> run parseHejOrHey |> should equal (Success((('h', 'e'), 'j'), ""))

[<Test>]
let ``Combine .>>. and <|> succeeds to parse "hey"`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'e' in
    let parseJ = pchar 'j' in
    let parseY = pchar 'y' in
    let parseHejOrHey = (parseH .>>. parseE) .>>. (parseJ <|> parseY) in
    "hey" |> run parseHejOrHey |> should equal (Success((('h', 'e'), 'y'), ""))

[<Test>]
let ``Combine .>>. and <|> fails in the "orElse" parser`` () =
    let parseH = pchar 'h' in
    let parseE = pchar 'e' in
    let parseJ = pchar 'j' in
    let parseY = pchar 'y' in
    let parseHejOrHey = (parseH .>>. parseE) .>>. (parseJ <|> parseY) in

    "hei"
    |> run parseHejOrHey
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'y'. Got: 'i'\""

[<Test>]
let ``choice succeeds with a parser from a list or parsers`` () =
    let parserList = [ pchar 'x'; pchar 'y'; pchar 'z' ] in
    "z" |> run (choice parserList) |> should equal (Success('z', ""))

[<Test>]
let ``choice fails with a parser from a list or parsers`` () =
    let parserList = [ pchar 'x'; pchar 'y'; pchar 'z' ] in

    "u"
    |> run (choice parserList)
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'z'. Got: 'u'\""

[<Test>]
let ``anyOf succeeds to parse a char from a list of chars`` () =
    let charList = [ 'a'; 'b'; 'c' ] in "c" |> run (anyOf charList) |> should equal (Success('c', ""))

[<Test>]
let ``anyOf fails to parse a char from a list of chars`` () =
    let charList = [ 'a'; 'b'; 'c' ] in

    "d"
    |> run (anyOf charList)
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'c'. Got: 'd'\""

[<Test>]
let ``parseLowercase succeeds to parse any lowercase char between a-z`` () =
    "a" |> run parseLowercase |> should equal (Success('a', ""))

[<Test>]
let ``parseLowercase fails to parse any non lowercase char between a-z`` () =
    "A"
    |> run parseLowercase
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'z'. Got: 'A'\""

[<Test>]
let ``parseDigit succeeds to parse any digit between 0-9`` () =
    "0" |> run parseDigit |> should equal (Success('0', ""))

[<Test>]
let ``parseDigit combined with 'map int' succeeds to parse any digit between 0-9 into a int`` () =
    // we need help from System.String to parse the char to a string
    let parseDigitAsInt = map (fun x -> System.String [| x |] |> int) parseDigit in
    run parseDigitAsInt "0" |> should equal (Success(0, ""))

[<Test>]
let ``map transforms the value inside a successfull parser`` () =
    let parseH = pchar 'h' in "h" |> run (map Char.ToUpper parseH) |> should equal (Success('H', ""))

[<Test>]
let ``<!> (map infix) transforms the value inside a successfull parser`` () =
    let parseH = pchar 'h' in "h" |> run (Char.ToUpper <!> parseH) |> should equal (Success('H', ""))

[<Test>]
let ``|>> (map reverse args) transforms the value inside a successfull parser`` () =
    let parseH = pchar 'h' in
    let parseHToUppercase = parseH |>> Char.ToUpper
    "h" |> run parseHToUppercase |> should equal (Success('H', ""))

[<Test>]
let ``map skips the value inside a failed parser`` () =
    let parseH = pchar 'h' in

    "j"
    |> run (map Char.ToUpper parseH)
    |> (fun x -> x.ToString())
    |> should equal "Failure \"Expected 'h'. Got: 'j'\""

[<Test>]
let ``retrn lifts a value into a parser`` () =
    run (pure' 'h') "" |> should equal (Success('h', ""))

[<Test>]
let ``apply calls a wrapped function with a value wrapped in another parser`` () =
    let fP = pure' Char.ToUpper in
    let xP = pure' 'x' in
    run (apply fP xP) "" |> should equal (Success('X', ""))

[<Test>]
let ``<*> (apply) calls a wrapped function with a value wrapped in another parser`` () =
    let fP = pure' Char.ToUpper in
    let xP = pure' 'x' in
    run (fP <*> xP) "" |> should equal (Success('X', ""))

[<Test>]
let ``lift2 (with ints) lifts a function of 2 arguments into a parser and can take two parsers of values to apply to that lifted function``
    ()
    =
    let liftedValue1 = (pure' 1)
    let liftedValue2 = (pure' 2)

    run (lift2 (+) liftedValue1 liftedValue2) "" |> should equal (Success(3, ""))

[<Test>]
let ``lift2 (with strings) lifts a function of 2 arguments into a parser and can take two parsers of values to apply to that lifted function``
    ()
    =
    let startsWith (prefix: string) (str: string) = str.StartsWith(prefix) in
    let liftedValue1 = pure' "hello"
    let liftedValue2 = pure' "hello world"

    run (lift2 startsWith liftedValue1 liftedValue2) ""
    |> should equal (Success(true, ""))
