[<AutoOpen>]
module Dap.Platform.Helper

open Elmish
open Fable.Core
open Fable.PowerPack

let utcNow = System.DateTime.UtcNow

type Second = float

let private sleepForSeconds (delay : Second) = promise {
    do! Promise.sleep <| int (delay * 1000.0)
    return delay
}

let addFutureCmd (delay : Second) (msg : 'msg) : Operate<'runner, 'model, 'msg> =
    Cmd.ofPromise
        sleepForSeconds delay
        (fun _ -> msg)
        (fun e ->
            Fable.Import.Browser.console.error ("addFutureCmd Failed:", [|box delay ; box msg |])
            Fable.Import.Browser.console.error ("Exception:", [|box e.Message ; box "\nStackTrace:" ; box e.StackTrace|])
            msg
        )
    |> addCmd'

let getWebSocketUri (path : string) : string =
    let url = Fable.Import.Browser.URL.Create(Fable.Import.Browser.window.location.href)
    let protocol = url.protocol.Replace ("http", "ws")
    sprintf "%s//%s/%s" protocol url.host path
