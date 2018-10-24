[<RequireQualifiedAccess>]
module Dap.Context.Meta.Context

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

type Builder (properties : string * ComboMeta) =
    inherit MetaBuilder<ContextMeta> ()
    new (properties' : Expr<ComboMeta>) =
        let properties' =
            unquotePropertyGetExpr properties'
        Builder (properties')
    override __.Zero () =
        ContextMeta.Create properties

    [<CustomOperation("kind")>]
    member __.Kind (context: ContextMeta, kind : Kind) =
        {context with Kind = Some kind}
    [<CustomOperation("channel")>]
    member __.Channel (context: ContextMeta, evt : FieldMeta) =
        ChannelMeta.Create evt
        |> context.AddChannel
    [<CustomOperation("handler")>]
    member __.Handler (context: ContextMeta, req : FieldMeta, res : FieldMeta) =
        HandlerMeta.Create req res
        |> context.AddHandler