[<RequireQualifiedAccess>]
module Dap.Context.Generator.Context

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
        ]
    let getInherits (param : ContextParam) =
        [
            sprintf "    inherit IContext<%s>" meta.PropsType
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
    let getAsyncHandler (_param : ContextParam) (handler : HandlerMeta) =
        if isFableGenerator then
            failWith "FableGenerator: AsyncHandler_Not_Supported" handler
        sprintf "    abstract %sAsync : IAsyncHandler<%s, %s> with get" handler.Req.Key.AsCodeMemberName handler.Req.ValueType handler.Res.ValueType
    let getAsyncHandlers (param : ContextParam) =
        meta.AsyncHandlers
        |> List.map ^<| getAsyncHandler param
    abstract member GetExtraInherits : ContextParam -> Lines
    default __.GetExtraInherits (_param : ContextParam) = []
    interface IGenerator<ContextParam> with
        member this.Generate param =
            [
                getInterfaceHeader param
                this.GetExtraInherits param
                getInherits param
                getProperties param
                getChannels param
                getHandlers param
                getAsyncHandlers param
            ]|> List.concat

type ClassGenerator (meta : ContextMeta) =
    let getClassHeader (param : ContextParam) =
        let props = meta.PropsType
        [
            if meta.Kind.IsSome then
                yield sprintf "[<Literal>]"
                yield sprintf "let %sKind = \"%s\"" param.Name meta.Kind.Value.AsCodeMemberName
                yield sprintf ""
            if meta.IsAbstract then
                yield sprintf "[<AbstractClass>]"
            match meta.Kind with
            | None ->
                if meta.IsAbstract then
                    yield sprintf "type Base%s<'context when 'context :> I%s> (kind : Kind, logging : ILogging) =" param.Name param.Name
                    yield sprintf "    inherit CustomContext<'context, ContextSpec<%s>, %s> (logging, new ContextSpec<%s>(kind, %s.Create))" props props props props
                else
                    yield sprintf "type %s (kind : Kind, logging : ILogging) =" param.Name
                    yield sprintf "    inherit CustomContext<%s, ContextSpec<%s>, %s> (logging, new ContextSpec<%s>(kind, %s.Create))" param.Name props props props props
            | Some kind ->
                if meta.IsAbstract then
                    yield sprintf "type Base%s<'context when 'context :> I%s> (logging : ILogging) =" param.Name param.Name
                    yield sprintf "    inherit CustomContext<'context, ContextSpec<%s>, %s> (logging, new ContextSpec<%s>(%sKind, %s.Create))" props props props param.Name props
                else
                    yield sprintf "type %s (logging : ILogging) =" param.Name
                    yield sprintf "    inherit CustomContext<%s, ContextSpec<%s>, %s> (logging, new ContextSpec<%s>(%sKind, %s.Create))" param.Name props props props param.Name props
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
    let getAsyncHandlerField (_param : ContextParam) (handler : HandlerMeta) =
        if isFableGenerator then
            failWith "FableGenerator: AsyncHandler_Not_Supported" handler
        sprintf "    let %sAsync = base.AsyncHandlers.Add<%s, %s> (%s, %s, %s, %s, \"%s\")"
            handler.Req.Key.AsCodeVariableName handler.Req.ValueType handler.Res.ValueType
            handler.Req.Encoder handler.Req.Decoder handler.Res.Encoder handler.Res.Decoder
            handler.Req.Key.AsCodeJsonKey
    let getAsyncHandlersField (param : ContextParam) =
        meta.AsyncHandlers
        |> List.map ^<| getAsyncHandlerField param
    let getOverrides (param : ContextParam) =
        if meta.IsAbstract then
            []
        else
            [
                yield sprintf "    static member Create (?logging : ILogging) ="
                yield sprintf "        let logging = logging |> Option.defaultWith (fun () -> getLogging ())"
                match meta.Kind with
                | None ->
                    yield sprintf "        new %s (kind, logging)" param.Name
                | Some _kind ->
                    yield sprintf "        new %s (logging)" param.Name
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
    let getAsyncHandlerMember (_param : ContextParam) (handler : HandlerMeta) =
        if isFableGenerator then
            failWith "FableGenerator: AsyncHandler_Not_Supported" handler
        sprintf "    member __.%sAsync : IAsyncHandler<%s, %s> = %sAsync" handler.Req.Key.AsCodeMemberName handler.Req.ValueType handler.Res.ValueType handler.Req.Key.AsCodeVariableName
    let getAsyncHandlersMember (param : ContextParam) =
        meta.AsyncHandlers
        |> List.map ^<| getAsyncHandlerMember param
    let getClassMiddle (param : ContextParam) =
        [
            sprintf "    interface I%s with" param.Name
        ]
    let getClassFooter (param : ContextParam) =
        [
            yield sprintf "    member this.As%s = this :> I%s" param.Name param.Name

        ]
    abstract member GetExtraInterfaces : ContextParam -> Lines
    default __.GetExtraInterfaces (_param : ContextParam) = []
    interface IGenerator<ContextParam> with
        member this.Generate param =
            [
                getClassHeader param
                getChannelsField param
                getHandlersField param
                getAsyncHandlersField param
                getOverrides param
                getPropertiesMember param
                getChannelsMember param
                getHandlersMember param
                getAsyncHandlersMember param
                getClassMiddle param
                getPropertiesMember param |> indentLines
                getChannelsMember param |> indentLines
                getHandlersMember param |> indentLines
                getAsyncHandlersMember param |> indentLines
                this.GetExtraInterfaces param
                getClassFooter param
            ]|> List.concat
