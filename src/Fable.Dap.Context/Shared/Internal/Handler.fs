[<AutoOpen>]
module Dap.Context.Internal.Handler

open Dap.Prelude
open Dap.Context

type internal Handler<'req, 'res> private (owner', spec') =
    let owner : IOwner = owner'
    let spec : IHandlerSpec<'req, 'res> = spec'
    let mutable ver = 0
    let mutable sealed' : bool = false
    let mutable muted = false
    let mutable handler : ('req -> 'res) option = None
    let onRequest = new Bus<RequestReceived<'req, 'res>> (owner, "OnRequest")
    let onRequest0 = new Bus<RequestReceived> (owner, "OnRequest0")
    let onResponse = new Bus<RequestHandled<'req, 'res>> (owner, "OnResponse")
    let onResponse0 = new Bus<RequestHandled> (owner, "OnResponse0")
    static member Create o s = new Handler<'req, 'res> (o, s)
    member this.AsHandler = this :> IHandler<'req, 'res>
    interface IAspect with
        member __.Owner = owner
        member __.Ver = ver
    interface IHandler with
        member __.Spec0 = spec :> IHandlerSpec
        member __.Seal () =
            if not sealed' then
                sealed' <- true
        member __.Sealed = sealed'
        member __.Muted = muted
        member __.SetMuted m = muted <- m
        member this.Handle0 (req : Json) =
            castJson spec.ReqDecoder req
            |> this.AsHandler.Handle
            |> spec.ResEncoder
        member __.OnRequest0 = onRequest0.Publish
        member __.OnResponse0 = onResponse0.Publish
    interface IHandler<'req, 'res> with
        member __.Spec = spec
        member __.SetHandler' (handler' : 'req -> 'res) =
            if sealed' then
                failWith "Handler_Sealed" <| sprintf "[%s] [%s]" spec.Luid spec.Key
            handler <- Some handler'
        member __.Handle (req : 'req) : 'res =
            if muted then
                logWarn owner "Handler" "Muted" req
                failWith "Handler_Muted" <| sprintf "[%s] [%s]" spec.Luid spec.Key
            else
                match handler with
                | None ->
                    logWarn owner "Handler" "Invalid" req
                    failWith "Handler_Invalid" <| sprintf "[%s] [%s]" spec.Luid spec.Key
                | Some handler' ->
                    ver <- ver + 1
                    let received : RequestReceived<'req, 'res> =
                        {
                            Spec = spec
                            Req = req
                        }
                    onRequest.Trigger received
                    if onRequest0.HasWatchers then
                        let received0 : RequestReceived =
                            {
                                Spec = spec :> IHandlerSpec
                                Req = spec.ReqEncoder req
                            }
                        onRequest0.Trigger received0
                    let res = handler' req
                    let handled : RequestHandled<'req, 'res> =
                        {
                            Spec = spec
                            Req = req
                            Res = res
                        }
                    onResponse.Trigger handled
                    if onResponse0.HasWatchers then
                        let handled0 : RequestHandled =
                            {
                                Spec = spec :> IHandlerSpec
                                Req = spec.ReqEncoder req
                                Res = spec.ResEncoder res
                            }
                        onResponse0.Trigger handled0
                    res
        member __.OnRequest = onRequest.Publish
        member __.OnResponse = onResponse.Publish
