[<AutoOpen>]
module Dap.Platform.Helper

let utcNow = System.DateTime.UtcNow

let getWebSocketUri (path : string) : string =
    let url = Fable.Import.Browser.URL.Create(Fable.Import.Browser.window.location.href)
    let protocol = url.protocol.Replace ("http", "ws")
    sprintf "%s//%s/%s" protocol url.host path