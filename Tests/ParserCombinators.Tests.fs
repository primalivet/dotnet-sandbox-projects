module Sandbox.ParserCombinatorsTests

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
