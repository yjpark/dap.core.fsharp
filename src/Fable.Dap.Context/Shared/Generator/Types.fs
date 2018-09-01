[<AutoOpen>]
module Dap.Context.Generator.Types

open Dap.Prelude
open Dap.Context

type Lines = string list

type IParam =
    abstract Kind : string with get
    abstract Name : string with get
    abstract Desc : string with get

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
        member __.Kind = "Record"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.IsJson then
                    yield "IsJson"
                    if this.IsLoose then
                        yield "IsLoose"
            ] |> String.concat ", "

type IRecordGenerator =
    abstract Generate : RecordParam -> Lines

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
        member __.Kind = "Class"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.IsFinal then
                    yield "IsFinal"
                if this.IsAbstract then
                    yield "IsAbstract"
            ] |> String.concat ", "

type IClassGenerator =
    abstract Generate : ClassParam -> Lines

type BuilderParam = {
    Name : string
    Kind : Kind
} with
    static member Create name kind =
        {
            Name = name
            Kind = kind
        }
    interface IParam with
        member __.Kind = "Builder"
        member this.Name = this.Name
        member this.Desc = ""

type IBuilderGenerator =
    abstract Generate : BuilderParam -> Lines

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
        member __.Kind = "Module"
        member this.Name = this.Name
        member this.Desc =
            [
                if this.AutoOpen then
                    yield "AutoOpen"
            ] |> String.concat ", "
