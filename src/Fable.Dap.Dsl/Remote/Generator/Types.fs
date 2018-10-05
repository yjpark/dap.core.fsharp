[<AutoOpen>]
module Dap.Remote.Generator.Types

open Dap.Context.Generator

type StubParam = {
    Name : string
} with
    static member Create name =
        {
            Name = name
        }
    interface IParam with
        member __.Category = "Stub"
        member this.Name = this.Name
        member this.Desc = ""

