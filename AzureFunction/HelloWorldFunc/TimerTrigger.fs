namespace Company.Function

open System
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Host
open Microsoft.Extensions.Logging

module TimerTrigger =
    [<FunctionName("TimerTrigger")>]
    let run([<TimerTrigger("0 */1 * * * *")>]myTimer: TimerInfo, log: ILogger) =
        let msg = sprintf "F# Time trigger function executed at: %A" DateTime.Now
        log.LogInformation msg
