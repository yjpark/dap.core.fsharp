[<RequireQualifiedAccess>]
module Dap.Context.Generator.Context

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator.Util

type ContextMeta with
    member this.PropsType =
        let (name, _meta) = this.Properties
        name.AsCodeMemberName

type InterfaceGenerator (meta : ContextMeta) =
    let getInterfaceHeader (param : ContextParam) =
        [
            sprintf "type I%s =" param.Name
            sprintf "    inherit IContext"
        ]
    let getProperties (_param : ContextParam) =
        let props = meta.PropsType
        [
            sprintf "    abstract %s : %s with get" props props
        ]
    let getChannel (_param : ContextParam) (channel : ChannelMeta) =
        sprintf "    abstract %s : IChannel<%s> with get" channel.Evt.Key.AsCodeMemberName channel.Evt.ValueType
    let getChannels (param : ContextParam) =
        meta.Channels
        |> List.map ^<| getChannel param
    let getHandler (_param : ContextParam) (handler : HandlerMeta) =
        sprintf "    abstract %s : IHandler<%s, %s> with get" handler.Req.Key.AsCodeMemberName handler.Req.ValueType handler.Res.ValueType
    let getHandlers (param : ContextParam) =
        meta.Handlers
        |> List.map ^<| getHandler param
    interface IGenerator<ContextParam> with
        member this.Generate param =
            [
                getInterfaceHeader param
                getProperties param
                getChannels param
                getHandlers param
            ]|> List.concat

type ClassGenerator (meta : ContextMeta) =
    let getClassHeader (param : ContextParam) =
        let props = meta.PropsType
        if meta.IsAbstract then
            [
                sprintf "[<AbstractClass>]"
            ]
        else
            []
        @
        match meta.Kind with
        | None ->
            [
                sprintf "type Base%s<'context when 'context :> I%s> (kind : Kind, logging : ILogging) =" param.Name param.Name
                sprintf "    inherit CustomContext<'context, ContextSpec<%s>, %s> (logging, new ContextSpec<%s>(kind, %s.Create))" props props props props
            ]
        | Some kind ->
            [
                sprintf "[<Literal>]"
                sprintf "let %sKind = \"%s\"" param.Name kind
                sprintf ""
                sprintf "type %s (logging : ILogging) =" param.Name
                sprintf "    inherit CustomContext<%s, ContextSpec<%s>, %s> (logging, new ContextSpec<%s>(%sKind, %s.Create))" param.Name props props props param.Name props
            ]
    let getChannelField (_param : ContextParam) (channel : ChannelMeta) =
        sprintf "    let %s = base.Channels.Add<%s> (%s, %s, \"%s\")"
            channel.Evt.Key.AsCodeVariableName channel.Evt.ValueType
            channel.Evt.Encoder channel.Evt.Decoder
            channel.Evt.Key.AsCodeJsonKey
    let getChannelsField (param : ContextParam) =
        meta.Channels
        |> List.map ^<| getChannelField param
    let getHandlerField (_param : ContextParam) (handler : HandlerMeta) =
        sprintf "    let %s = base.Handlers.Add<%s, %s> (%s, %s, %s, %s, \"%s\")"
            handler.Req.Key.AsCodeVariableName handler.Req.ValueType handler.Res.ValueType
            handler.Req.Encoder handler.Req.Decoder handler.Res.Encoder handler.Res.Decoder
            handler.Req.Key.AsCodeJsonKey
    let getHandlersField (param : ContextParam) =
        meta.Handlers
        |> List.map ^<| getHandlerField param
    let getOverrides (param : ContextParam) =
        if meta.IsAbstract then
            []
        else
            [
                yield sprintf "    override this.Self = this"
                match meta.Kind with
                | None ->
                    yield sprintf "    override __.Spawn l = new %s (kind, l)" param.Name
                | Some _kind ->
                    yield sprintf "    override __.Spawn l = new %s (l)" param.Name
            ]
    let getPropertiesMember (_param : ContextParam) =
        let props = meta.PropsType
        [
            sprintf "    member this.%s : %s = this.Properties" props props
        ]
    let getChannelMember (_param : ContextParam) (channel : ChannelMeta) =
        sprintf "    member __.%s : IChannel<%s> = %s" channel.Evt.Key.AsCodeMemberName channel.Evt.ValueType channel.Evt.Key.AsCodeVariableName
    let getChannelsMember (param : ContextParam) =
        meta.Channels
        |> List.map ^<| getChannelMember param
    let getHandlerMember (_param : ContextParam) (handler : HandlerMeta) =
        sprintf "    member __.%s : IHandler<%s, %s> = %s" handler.Req.Key.AsCodeMemberName handler.Req.ValueType handler.Res.ValueType handler.Req.Key.AsCodeVariableName
    let getHandlersMember (param : ContextParam) =
        meta.Handlers
        |> List.map ^<| getHandlerMember param
    let getClassMiddle (param : ContextParam) =
        [
            sprintf "    interface I%s with" param.Name
        ]
    let getClassFooter (param : ContextParam) =
        [
            sprintf "    member this.As%s = this :> I%s" param.Name param.Name
        ]
    interface IGenerator<ContextParam> with
        member this.Generate param =
            [
                getClassHeader param
                getChannelsField param
                getHandlersField param
                getOverrides param
                getPropertiesMember param
                getChannelsMember param
                getHandlersMember param
                getClassMiddle param
                getPropertiesMember param |> indentLines
                getChannelsMember param |> indentLines
                getHandlersMember param |> indentLines
                getClassFooter param
            ]|> List.concat
