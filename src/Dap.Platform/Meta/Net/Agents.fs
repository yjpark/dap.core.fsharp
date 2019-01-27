[<AutoOpen>]
module Dap.Platform.Meta.Net.Agents

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Meta.Util
open Dap.Platform
open Dap.Platform.Meta
module TickerTypes = Dap.Platform.Ticker.Types
module RegistryTypes = Dap.Platform.Registry.Types

type M with
    static member ticker (?args : ArgsMeta, ?kind : Kind, ?key : Key) =
        let args = defaultArg args <| JsonArgs "TickerTypes.Args"
        let kind = defaultArg kind TickerTypes.Kind
        let alias = "TickerTypes", "Dap.Platform.Ticker.Types"
        let type' = "TickerTypes.Agent"
        let spec = "Dap.Platform.Ticker.Logic.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = [alias])
    static member ticker (tickerArgs : TickerArgs, ?kind : Kind, ?key : Key) =
        let args = jsonCodeArgs "TickerTypes.Args" TickerArgs.JsonEncoder tickerArgs
        M.ticker (args, ?kind = kind, ?key = key)

type M with
    static member registry (kv : string, ?kind : Kind, ?key : Key, ?aliases : ModuleAlias list) =
        let kind = defaultArg kind RegistryTypes.Kind
        let aliases = defaultArg aliases []
        let alias = "RegistryTypes", "Dap.Platform.Registry.Types"
        let args = CodeArgs "NoArgs" "NoArgs"
        let type' = sprintf "RegistryTypes.Agent<%s>" kv
        let spec = "Dap.Platform.Registry.Logic.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = alias :: aliases)