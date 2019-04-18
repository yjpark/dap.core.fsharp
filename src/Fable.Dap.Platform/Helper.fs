[<AutoOpen>]
module Dap.Platform.Helper

open Fable.Core

let promise = new Promise.PromiseBuilder ()

let utcNow = System.DateTime.UtcNow

let private sleepForSeconds (delay : float<second>) = promise {
    do! Promise.sleep <| int (1000.0 * float delay)
    return delay
}

let addFutureCmd (delay : float<second>) (msg : 'msg) : Operate<'runner, 'model, 'msg> =
    Elmish.Cmd.OfPromise.either
        sleepForSeconds delay
        (fun _ -> msg)
        (fun e ->
            Browser.Dom.console.error ("addFutureCmd Failed:", [|box delay ; box msg |])
            Browser.Dom.console.error ("Exception:", [|box e.Message ; box "\nStackTrace:" ; box e.StackTrace|])
            msg
        )
    |> addCmd'

let getWebSocketUri (path : string) : string =
    let url = Browser.Url.URL.Create(Browser.Dom.window.location.href)
    let protocol = url.protocol.Replace ("http", "ws")
    sprintf "%s//%s/%s" protocol url.host path
