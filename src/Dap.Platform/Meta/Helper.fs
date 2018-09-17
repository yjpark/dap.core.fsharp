[<AutoOpen>]
module Dap.Platform.Meta.Helper'

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Meta.Util
open Dap.Platform
module TickerTypes = Dap.Platform.Ticker.Types
module RegistryTypes = Dap.Platform.Registry.Types

type M with
    static member tickerService (args : ArgsMeta, kind : Kind, key : Key) =
        let alias = "TickerTypes", "Dap.Platform.Ticker.Types"
        let type' = "TickerTypes.Agent"
        let spec = "Dap.Platform.Ticker.Logic.spec"
        M.service ([alias], args, type', spec, kind, key)
    static member tickerService (kind : Kind, key : Key) =
        let args = JsonArgs "TickerTypes.Args"
        M.tickerService (args, kind, key)
    static member tickerService (key : Key) =
        M.tickerService (TickerTypes.Kind, key)
    static member tickerService () =
        M.tickerService (NoKey)
    static member tickerService (tickerArgs : TickerArgs, kind : Kind, key : Key) =
        let args = jsonCodeArgs "TickerTypes.Args" TickerArgs.JsonEncoder tickerArgs
        M.tickerService (args, kind, key)
    static member tickerService (tickerArgs : TickerArgs, key : Key) =
        M.tickerService (tickerArgs, TickerTypes.Kind, key)

type M with
    static member registryService (aliases : ModuleAlias list, kv : string, kind : Kind, key : Key) =
        let alias = "RegistryTypes", "Dap.Platform.Registry.Types"
        let args = CodeArgs "NoArgs" "NoArgs"
        let type' = sprintf "RegistryTypes.Agent<%s>" kv
        let spec = "Dap.Platform.Registry.Logic.spec"
        M.service (alias :: aliases, args, type', spec, kind, key)
    static member registryService (aliases : ModuleAlias list, kv : string, key : Key) =
        M.registryService (aliases, kv, RegistryTypes.Kind, key)
    static member registryService (aliases : ModuleAlias list, kv : string) =
        M.registryService (aliases, kv, NoKey)
