[<AutoOpen>]
module Dap.Platform.Generator.Types

open Dap.Context.Generator

type PackParam = {
    Name : string
} with
    static member Create name : PackParam =
        {
            Name = name
        }
    interface IParam with
        member __.Category = "Pack"
        member this.Name = this.Name
        member this.Desc = ""

type AppParam = {
    Name : string
} with
    static member Create name : AppParam =
        {
            Name = name
        }
    interface IParam with
        member __.Category = "App"
        member this.Name = this.Name
        member this.Desc = ""
