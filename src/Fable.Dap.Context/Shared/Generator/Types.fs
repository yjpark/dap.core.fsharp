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

type Interface = {
    Name : string
    Template : IObj
} with
    static member Create name template =
        {
            Name = name
            Template = template
        }

type InterfaceParam = {
    Name : string
} with
    static member Create name =
        {
            Name = name
        }
    interface IParam with
        member __.Category = "Interface"
        member this.Name = this.Name
        member this.Desc = ""

type RecordParam = {
    Name : string
    IsJson : bool
    IsLoose : bool
    Interfaces : Interface list
} with
    static member Create name isJson isLoose interfaces =
        {
            Name = name
            IsJson = isJson
            IsLoose = isLoose
            Interfaces = interfaces
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
                for face in this.Interfaces do
                    yield face.Name
            ] |> String.concat ", "

type ClassParam = {
    Name : string
    Kind : Kind
    IsAbstract : bool
    IsFinal : bool
    Interfaces : Interface list
} with
    static member Create name kind isAbstract isFinal interfaces =
        {
            Name = name
            Kind = kind
            IsAbstract = isAbstract
            IsFinal = isFinal
            Interfaces = interfaces
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
                for face in this.Interfaces do
                    yield face.Name
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
