namespace FSAzureFns.Function

open Microsoft.AspNetCore.Mvc
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging

module Helpers =
    let parseQueryParam (key: string) (req: HttpRequest) = req.Query.[key] |> Seq.tryHead

    let tryFloat (s: string) =
        try
            Some(float s)
        with _ ->
            None

module Converter =
    type Unit =
        | Celcius
        | Fahrenheit
        | Kilometer
        | Mile
        | Meter

    let tryParseUnit =
        function
        | "celcius" -> Some Celcius
        | "fahrenheit" -> Some Fahrenheit
        | "kilometer" -> Some Kilometer
        | "mile" -> Some Mile
        | "meter" -> Some Meter
        | _ -> None

    let celciusToFahrenheit (c: float) = c * 1.8 + 32.0
    let fahrenheitToCelcius (f: float) = (f - 32.0) / 1.8
    let kilometerToMeter (k: float) = k * 1000.0
    let kilometerToMile (k: float) = k * 1.609
    let meterToKilometer (m: float) = m / 1000.0
    let meterToMile (m: float) = m / 0.000621371
    let mileToKilometer (m: float) = m * 1.609
    let mileToMeter (m: float) = m * 1609.34

    let formatConverted fromUnit toUnit value converted : string =
        sprintf "%f in %s converted to %s is %f" value fromUnit toUnit converted

    let formatDontKnow fromUnit toUnit : string =
        sprintf "Dont know how to convert from %A to %A" fromUnit toUnit

    let formatConversion (fromUnit, toUnit) x =
        match fromUnit, toUnit with
        | Celcius, Fahrenheit -> formatConverted "Celcius" "Fahrenheit" x (celciusToFahrenheit x)
        | Fahrenheit, Celcius -> formatConverted "Fahrenheit" "Celcius" x (fahrenheitToCelcius x)
        | Kilometer, Mile -> formatConverted "Kilometer" "Mile" x (kilometerToMile x)
        | Kilometer, Meter -> formatConverted "Kilometer" "Meter" x (kilometerToMeter x)
        | Meter, Kilometer -> formatConverted "Meter" "Kilometer" x (meterToKilometer x)
        | Meter, Mile -> formatConverted "Meter" "Mile" x (meterToMile x)
        | Mile, Kilometer -> formatConverted "Mile" "Kilometer" x (mileToKilometer x)
        | Mile, Meter -> formatConverted "Mile" "Meter" x (mileToMeter x)
        | fromUnit, toUnit -> formatDontKnow fromUnit toUnit



module Program =
    open Converter
    open Helpers

    [<FunctionName("unitconverter")>]
    let run
        ([<HttpTrigger(AuthorizationLevel.Function, "get", "post", Route = null)>] req: HttpRequest)
        (log: ILogger)
        =
        async {
            log.LogInformation("F# HTTP trigger function processed a request.")

            let fromUnit = req |> parseQueryParam "from" |> Option.bind tryParseUnit
            let toUnit = req |> parseQueryParam "to" |> Option.bind tryParseUnit
            let value = req |> parseQueryParam "value" |> Option.bind tryFloat

            let converted =
                match fromUnit, toUnit, value with
                | Some fromUnit, Some toUnit, Some value -> formatConversion (fromUnit, toUnit) value
                | _ -> "Missing query parameters, or invalid units"

            return OkObjectResult(converted) :> IActionResult
        }
        |> Async.StartAsTask
