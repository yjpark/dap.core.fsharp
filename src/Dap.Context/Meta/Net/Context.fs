[<AutoOpen>]
module Dap.Context.Meta.Net.Context

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util
open Dap.Context.Meta

type Dap.Context.Meta.Context.Builder with
    [<CustomOperation("async_handler")>]
    member __.AsyncHandler (context: ContextMeta, req : FieldMeta, res : FieldMeta) =
        HandlerMeta.Create req res
        |> context.AddAsyncHandler
