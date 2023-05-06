type TraceBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> printfn "Binding with None. Exiting"
        | Some x -> printfn "Binding with Some(%A). Continuing" x

        Option.bind f x

    member this.Return(x) =
        printfn "Wrapping %A as Some(%A)" x x
        Some x

    member this.ReturnFrom m =
        printfn "Returning a option (%A) directly" m
        m

let trace = TraceBuilder()

trace { return 1 } |> printfn "R1: %A"

trace { return! Some(1) } |> printfn "R2: %A"

trace {
    let! x = Some(1)
    let! y = Some(2)
    return x + y
}
|> printfn "R3: %A"
