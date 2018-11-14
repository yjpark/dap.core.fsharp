[<AutoOpen>]
module Dap.Context.Internal.Handlers

open System

open Dap.Prelude
open Dap.Context

type internal Handlers internal (owner', spec') =
    let owner : IOwner = owner'
    let spec : IAspectSpec = spec'
    let mutable value : IHandler List = []
    let mutable ver = 0
    let mutable sealed' : bool = false
    let onAdded = new Bus<IHandler> (owner, "OnAdded")
    let checkAdd (subSpec : IHandlerSpec) (reqType : Type) (resType : Type) =
        if sealed' then
            failWith "Handlers_Sealed" <| sprintf "[%s] <%s, %s> [%s]" spec.Luid reqType.FullName resType.FullName subSpec.Key
        value
        |> List.tryFind (fun ch -> ch.Key = subSpec.Key)
        |> Option.iter (fun ch ->
            failWith "Key_Exist" <| sprintf "[%s] <%s, %s> [%s] -> %A" spec.Luid reqType.FullName resType.FullName subSpec.Luid ch
        )
    member __.Value = value
    member __.Ver = ver
    member __.Sealed = sealed'
    member this.AsHandlers = this :> IHandlers
    interface IAspect with
        member __.Owner = owner
        member __.Ver = ver
        member __.SpecA = spec
    interface IValue<IHandler list> with
        member __.Value = value
    interface IHandlers with
        member __.Seal () =
            if not sealed' then
                sealed' <- true
        member __.Sealed = sealed'
        member __.TryGet k =
            value
            |> List.tryFind (fun ch -> k = ch.Key)
        member this.Has k =
            (this.AsHandlers.TryGet k).IsSome
        member this.Get k =
            this.AsHandlers.TryGet k
            |> function
                | Some ch -> ch
                | None -> failWith "Not_Found" k
        member this.Add<'req, 'res> (subSpec : IHandlerSpec<'req, 'res>) =
            checkAdd subSpec typeof<'req> typeof<'res>
            subSpec.AsSubSpec spec
            |> Handler<'req, 'res>.Create owner
            |> fun h ->
                let h' = h :> IHandler
                value <- value @ [h']
                onAdded.Trigger (h')
                h.AsHandler
        member __.OnAdded = onAdded.Publish


