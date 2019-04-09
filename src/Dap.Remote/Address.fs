[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Remote.Address

open System.Net

open Dap.Prelude

let getAddress (host : string) : Option<IPAddress> =
    if System.String.IsNullOrEmpty host then
        None
    else
        try
            let hostInfo = Dns.GetHostEntry (host)
            hostInfo.AddressList
            |> Array.tryFind (fun ip ->
                ip.AddressFamily = Sockets.AddressFamily.InterNetwork
            )
        with e ->
            logException (getLogging ()) "getIp" "Failed" (host) e
            None

let getHostName (host : string option) =
    host
    |> Option.defaultWith Dns.GetHostName

let getHostIp (host : string option) : string =
    (*
    * you can put the ip for the hostname into
    * /etc/hosts manually to override dns lookup
    *)
    host
    |> Option.bind (fun host ->
        getAddress host
    )|> Option.defaultWith (fun () ->
        getAddress <| Dns.GetHostName ()
        |> Option.defaultValue (IPAddress.Parse ("127.0.0.1"))
    )
    |> fun address -> address.ToString ()
