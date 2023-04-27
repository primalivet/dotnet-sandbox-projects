module Sandbox.ParserCombinators

(*
  The below code is an reimplementation for practive purposes of the 
  exellent "parser combinators from scratch" by Scrott Wlachin 
  Link: https://fsharpforfunandprofit.com
*)

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

let pchar c =
    let innerFn s =
        match s |> Seq.toList with
        | [] -> Failure "No more input"
        | s :: ss when c = s ->
            let remaining = ss |> List.map (fun x -> x.ToString()) |> String.concat "" in Success(s, remaining)
        | s :: _ -> let reason = sprintf "Expected '%c'. Got: '%c'" c s in Failure reason

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
        | Failure _ -> run parser2 input

    Parser innerFn

let (<|>) = orElse

let choice parserList = List.reduce (<|>) parserList

let anyOf charList = charList |> List.map pchar |> choice

let parseLowercase = anyOf [ 'a' .. 'z' ]

let parseDigit = anyOf [ '0' .. '9' ]

let map f parser =
    let innerFn input =
        let result = run parser input

        match result with
        | Failure reason -> Failure reason
        | Success(value, remaining) -> Success(f value, remaining)

    Parser innerFn

let (<!>) = map

// Reverse arguments to map for composition
let (|>>) x f = map f x

// return and pure seem like reserved words
let pure' x =
    let innerFn input = Success(x, input)
    Parser innerFn

// NOTE: The below stuff on lifting function and applying wrapped values
// to wrapped hard to wrap your head around :)
//
// But the implementation should not be the focus, as with all functional
// programming the abstraction is the important thing.
// In 'apply' the general concept is that we take a function wrapped in a
// Parser as well as a argument also wrapped in a Parser, then we
// "take them out" and apply the value to the function and then wrapp
// them back up
//
// And in general we should think more about the type signatures than the
// acctual implementations as the functions build on top of each other.
//
// Like apply builds on top of both 'andThen' and 'map'


// Identical implementation of apply
// let apply fP xP = andThen fP xP |> map (fun (f, x) -> f x)
let apply fP xP = fP .>>. xP |>> fun (f, x) -> f x
let (<*>) = apply


// Identical implementation of lift2
// let lift2 f xP yP = apply (apply (pure' f) xP) yP
let lift2 f xP yP = pure' f <*> xP <*> yP
