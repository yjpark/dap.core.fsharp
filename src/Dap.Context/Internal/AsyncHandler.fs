[<AutoOpen>]
module Dap.Context.Internal.AsyncHandler

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2

open Dap.Prelude
open Dap.Context

type internal AsyncHandler<'req, 'res> private (owner', spec') =
    let owner : IOwner = owner'
    let spec : IHandlerSpec<'req, 'res> = spec'
    let mutable ver = 0
    let mutable sealed' : bool = false
    let mutable muted = false
    let mutable handler : ('req -> Task<'res>) option = None
    let onRequest = new Bus<RequestReceived<'req, 'res>> (owner, "OnRequest")
    let onRequest0 = new Bus<RequestReceived> (owner, "OnRequest0")
    let onResponse = new Bus<RequestHandled<'req, 'res>> (owner, "OnResponse")
    let onResponse0 = new Bus<RequestHandled> (owner, "OnResponse0")
    let handleAsync (req : 'req) : Task<'res * Json option> = task {
        if muted then
            logWarn owner "Handler" "Muted" req
            failWith "Handler_Muted" <| sprintf "[%s] [%s]" spec.Luid spec.Key
            let res = castJson spec.ResDecoder E.nil
            return (res, None)
        else
            match handler with
            | None ->
                logWarn owner "Handler" "Invalid" req
                failWith "Handler_Invalid" <| sprintf "[%s] [%s]" spec.Luid spec.Key
                let res = castJson spec.ResDecoder E.nil
                return (res, None)
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
                let! res = handler' req
                let handled : RequestHandled<'req, 'res> =
                    {
                        Spec = spec
                        Req = req
                        Res = res
                    }
                onResponse.Trigger handled
                let res0 =
                    if onResponse0.HasWatchers then
                        let res0 = spec.ResEncoder res
                        let handled0 : RequestHandled =
                            {
                                Spec = spec :> IHandlerSpec
                                Req = spec.ReqEncoder req
                                Res = res0
                            }
                        onResponse0.Trigger handled0
                        Some res0
                    else
                        None
                return (res, res0)
    }
    static member Create o s = new AsyncHandler<'req, 'res> (o, s)
    member this.AsHandler = this :> IAsyncHandler<'req, 'res>
    interface IAspect with
        member __.Owner = owner
        member __.Ver = ver
        member __.SpecA = spec :> IAspectSpec
    interface IAsyncHandler with
        member __.Spec0 = spec :> IHandlerSpec
        member __.Seal () =
            if not sealed' then
                sealed' <- true
        member __.Sealed = sealed'
        member __.Muted = muted
        member __.SetMuted m = muted <- m
        member this.Handle0 (req : Json) : Task<Json> = task {
            let req = castJson spec.ReqDecoder req
            let! (res, res0) = handleAsync req
            let res0 = res0 |> Option.defaultWith (fun () -> spec.ResEncoder res)
            return res0
        }
        member __.OnRequest0 = onRequest0.Publish
        member __.OnResponse0 = onResponse0.Publish
    interface IAsyncHandler<'req, 'res> with
        member __.Spec = spec
        member __.SetupHandler' (handler' : 'req -> Task<'res>) =
            if sealed' then
                failWith "Handler_Sealed" <| sprintf "[%s] [%s]" spec.Luid spec.Key
            handler <- Some handler'
        member __.Handle (req : 'req) : Task<'res> = task {
            let! (res, _res0) = handleAsync req
            return res
        }
        member __.OnRequest = onRequest.Publish
        member __.OnResponse = onResponse.Publish
