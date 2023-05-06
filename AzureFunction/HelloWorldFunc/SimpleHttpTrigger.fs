namespace FSAzureFns.Function

open System
open System.IO
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.Extensions.Logging

module SimpleHttpTrigger =
  [<FunctionName("SimpleHttpTrigger")>]
  let run ([<HttpTrigger(AuthorizationLevel.Function, "get", "post", Route = null)>]req:HttpRequest, log:ILogger) =
    async {
      log.LogInformation("F# HTTP trigger function processed a request.")
      let responseMessage = "Hello World!"
      return OkObjectResult(responseMessage) :> IActionResult
    } |> Async.StartAsTask
