[<AutoOpen>]
module Dap.Context.Internal.Channels

open System

open Dap.Prelude
open Dap.Context

type internal Channels internal (owner', spec') =
    let owner : IOwner = owner'
    let spec : IAspectSpec = spec'
    let mutable value : IChannel List = []
    let mutable ver = 0
    let mutable sealed' : bool = false
    let onAdded = new Bus<IChannel> (owner, "OnAdded")
    let checkAdd (subSpec : IChannelSpec) (evtType : Type) =
        if sealed' then
            failWith "Channels_Sealed" <| sprintf "[%s] <%s> [%s]" spec.Luid evtType.FullName subSpec.Key
        value
        |> List.tryFind (fun ch -> ch.Spec0.Key = subSpec.Key)
        |> Option.iter (fun ch ->
            failWith "Key_Exist" <| sprintf "[%s] <%s> [%s] -> %A" spec.Luid evtType.FullName subSpec.Luid ch
        )
    member __.Value = value
    member __.Ver = ver
    member __.Sealed = sealed'
    member this.AsChannels = this :> IChannels
    interface IAspect with
        member __.Owner = owner
        member __.Ver = ver
    interface IValue<IChannel list> with
        member __.Value = value
    interface IChannels with
        member __.Seal () =
            if not sealed' then
                sealed' <- true
        member __.Sealed = sealed'
        member __.TryGet k =
            value
            |> List.tryFind (fun ch -> k = ch.Spec0.Key)
        member this.Has k =
            (this.AsChannels.TryGet k).IsSome
        member this.Get k =
            this.AsChannels.TryGet k
            |> function
                | Some ch -> ch
                | None -> failWith "Not_Found" k
        member this.Add<'evt> (subSpec : IChannelSpec<'evt>) =
            checkAdd subSpec typeof<'evt>
            subSpec.AsSubSpec spec
            |> Channel<'evt>.Create owner
            |> fun ch ->
                let ch' = ch :> IChannel
                value <- value @ [ch']
                onAdded.Trigger (ch')
                ch.AsChannel
        member __.OnAdded = onAdded.Publish


