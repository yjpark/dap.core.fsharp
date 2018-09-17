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

type InterfaceType =
    | ComboInterface
    | ValueInterface

type InterfaceParam = {
    Type : InterfaceType
    Name : string
} with
    static member Create type' name =
        {
            Type = type'
            Name = name
        }
    interface IParam with
        member this.Category =
            match this.Type with
            | ComboInterface -> "ComboInterface"
            | ValueInterface -> "ValueInterface"
        member this.Name = this.Name
        member __.Desc = ""

type Interface = {
    Param : InterfaceParam
    Meta : obj
} with
    static member CreateCombo name meta =
        {
            Param = InterfaceParam.Create ComboInterface name
            Meta = meta
        }
    static member CreateValue name meta =
        {
            Param = InterfaceParam.Create ValueInterface name
            Meta = meta
        }

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
                    yield face.Param.Name
            ] |> String.concat ", "

type UnionParam = {
    Name : string
    IsJson : bool
} with
    static member Create name isJson =
        {
            Name = name
            IsJson = isJson
        }
    interface IParam with
        member __.Category = "Union"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.IsJson then
                    yield "IsJson"
            ] |> String.concat ", "

type ClassParam = {
    Name : string
    IsAbstract : bool
    IsFinal : bool
    Interfaces : Interface list
} with
    static member Create name isAbstract isFinal interfaces =
        {
            Name = name
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
                    yield face.Param.Name
            ] |> String.concat ", "

type BuilderType =
    | ComboBuilder
    | ValueBuilder

type BuilderParam = {
    Type : BuilderType
    Name : string
    Kind : Kind
    Key : Key
} with
    static member Create type' name kind key =
        {
            Type = type'
            Name = name
            Kind = kind
            Key = key
        }
    interface IParam with
        member this.Category =
            match this.Type with
            | ComboBuilder -> "ComboBuilder"
            | ValueBuilder -> "ValueBuilder"
        member this.Name = this.Name
        member this.Desc = ""

type ModuleParam = {
    Name : string
    AutoOpen : bool
    RequireQualifiedAccess : bool
} with
    static member Create name autoOpen requireQualifiedAccess =
        {
            Name = name
            AutoOpen = autoOpen
            RequireQualifiedAccess = requireQualifiedAccess
        }
    interface IParam with
        member __.Category = "Module"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.AutoOpen then
                    yield "AutoOpen"
                if this.RequireQualifiedAccess then
                    yield "RequireQualifiedAccess"
            ] |> String.concat ", "
