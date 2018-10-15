[<AutoOpen>]
module Dap.Context.Internal.Channel

open Dap.Prelude
open Dap.Context

type internal Channel<'evt> private (owner', spec') =
    let owner : IOwner = owner'
    let spec : IChannelSpec<'evt> = spec'
    let mutable ver = 0
    let mutable muted = false
    let onEvent = new Bus<EventFired<'evt>> (owner, "OnEvent")
    let onEvent0 = new Bus<EventFired> (owner, "OnEvent0")
    static member Create o s = new Channel<'evt> (o, s)
    member this.AsChannel = this :> IChannel<'evt>
    interface IAspect with
        member __.Owner = owner
        member __.Ver = ver
    interface IChannel with
        member __.Spec0 = spec :> IChannelSpec
        member __.Muted = muted
        member __.SetMuted m = muted <- m
        member this.FireEvent0 (evt : Json) =
            castJson spec.Decoder evt
            |> this.AsChannel.FireEvent
        member __.OnEvent0 = onEvent0.Publish
    interface IChannel<'evt> with
        member __.Spec = spec
        member __.FireEvent (evt : 'evt) =
            if muted then
                logWarn owner "Channel" "Muted" evt
            else
                ver <- ver + 1
                let evt' : EventFired<'evt> =
                    {
                        Spec = spec
                        Evt = evt
                    }
                onEvent.Trigger evt'
                if onEvent0.HasWatchers then
                    let evt0 : EventFired =
                        {
                            Spec = spec :> IChannelSpec
                            Evt = spec.Encoder evt
                        }
                    onEvent0.Trigger evt0
        member __.OnEvent = onEvent.Publish

