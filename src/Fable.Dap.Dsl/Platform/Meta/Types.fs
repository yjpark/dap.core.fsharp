[<AutoOpen>]
module Dap.Platform.Meta.Types

open Dap.Prelude
open Dap.Context

type SinkMeta = {
    Kind : string
    MinLevel : LogLevel
    OtherArgs : string
} with
    static member Create kind minLevel otherArgs =
        {
            Kind = kind
            MinLevel = minLevel
            OtherArgs = otherArgs
        }

type LoggingMeta = {
    Sinks : SinkMeta list
} with
    static member Create sinks =
        {
            Sinks = sinks
        }

type ModuleAlias = string * string          //alias * full

type ArgsMeta =
    | JsonArgs of string
    | CodeArgs of string * string
with
    member this.Type =
        match this with
        | JsonArgs name -> name
        | CodeArgs (name, _code) -> name

let CodeArgs name code =
    CodeArgs (name, code)

type AgentMeta = {
    Aliases : ModuleAlias list
    Args : ArgsMeta
    Type : string
    Spec : string
    Pack : string option
    Kind : Kind
    Key : Key
} with
    static member Create aliases args type' spec pack kind key =
        {
            Aliases = aliases
            Args = args
            Type = type'
            Spec = spec
            Pack = pack
            Kind = kind
            Key = key
        }

type ExtraArgsMeta = {
    Aliases : ModuleAlias list
    Args : ArgsMeta
    Key : string
} with
    static member Create aliases args key =
        {
            Aliases = aliases
            Args = args
            Key = key
        }

type PackMeta = {
    Parents : (string * PackMeta) list
    Services : AgentMeta list
    Spawners : AgentMeta list
    ExtraArgs : ExtraArgsMeta list
} with
    static member Create parents services spawners extraArgs =
        {
            Parents = parents
            Services = services
            Spawners = spawners
            ExtraArgs = extraArgs
        }

type AppMeta = {
    Kind : string
    Platform : string
    Packs : (string * PackMeta) list
} with
    static member Create kind platform packs =
        {
            Kind = kind
            Platform = platform
            Packs = packs
        }