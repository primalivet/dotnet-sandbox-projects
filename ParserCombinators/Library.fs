module Sandbox.ParserCombinators

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

let pchar c =
    let innerFn s =
        match s |> Seq.toList with
        | [] -> Failure "No more input"
        | s :: ss when c = s -> Success(s, ss |> List.map (fun x -> x.ToString()) |> String.concat "")
        | s :: _ -> sprintf "Expected '%c'. Got: '%c'" c s |> Failure

    Parser innerFn

let run parser input =
    let (Parser innerFn) = parser
    innerFn input


let andThen parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input

        match result1 with
        | Failure reason -> Failure reason
        | Success(value1, remaining1) ->
            let result2 = run parser2 remaining1

            match result2 with
            | Failure reason -> Failure reason
            | Success(value2, remaining2) ->
                let newValue = (value1, value2)
                Success(newValue, remaining2)

    Parser innerFn

let (.>>.) = andThen

let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input

        match result1 with
        | Success _ -> result1
        | Failure reason -> run parser2 input

    Parser innerFn

let (<|>) = orElse

let choice parserList = List.reduce (<|>) parserList

let anyOf charList = charList |> List.map pchar |> choice

let parseLowercase = anyOf [ 'a' .. 'z' ]

let parseDigit = anyOf [ '0' .. '9' ]
