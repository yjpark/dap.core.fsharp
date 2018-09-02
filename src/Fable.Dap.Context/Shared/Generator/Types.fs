[<AutoOpen>]
module Dap.Context.Generator.Types

open Dap.Prelude
open Dap.Context

type Lines = string list

type IParam =
    abstract Category : string with get
    abstract Name : string with get
    abstract Desc : string with get

type IGenerator<'param when 'param :> IParam> =
    abstract Generate : 'param -> Lines

type RecordParam = {
    Name : string
    IsJson : bool
    IsLoose : bool
} with
    static member Create name isJson isLoose =
        {
            Name = name
            IsJson = isJson
            IsLoose = isLoose
        }
    interface IParam with
        member __.Category = "Record"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.IsJson then
                    yield "IsJson"
                    if this.IsLoose then
                        yield "IsLoose"
            ] |> String.concat ", "

type ClassParam = {
    Name : string
    Kind : Kind
    IsAbstract : bool
    IsFinal : bool
} with
    static member Create name kind isAbstract isFinal =
        {
            Name = name
            Kind = kind
            IsAbstract = isAbstract
            IsFinal = isFinal
        }
    interface IParam with
        member __.Category = "Class"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.IsFinal then
                    yield "IsFinal"
                if this.IsAbstract then
                    yield "IsAbstract"
            ] |> String.concat ", "

type BuilderParam = {
    Key : Key
    Name : string
    Kind : Kind
} with
    static member Create key name kind =
        {
            Key = key
            Name = name
            Kind = kind
        }
    interface IParam with
        member __.Category = "Builder"
        member this.Name = this.Name
        member this.Desc = ""

type ModuleParam = {
    Name : string
    AutoOpen : bool
} with
    static member Create name autoOpen =
        {
            Name = name
            AutoOpen = autoOpen
        }
    interface IParam with
        member __.Category = "Module"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.AutoOpen then
                    yield "AutoOpen"
            ] |> String.concat ", "
