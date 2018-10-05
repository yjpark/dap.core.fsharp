[<AutoOpen>]
module Dap.Remote.Meta.Types

open Dap.Context.Meta

type StubFieldKind =
    | Do
    | On
    | To
    | By

type StubFieldMeta = {
    Kind : StubFieldKind
    Type : string
} with
    static member Create kind type' : StubFieldMeta =
        {
            Kind = kind
            Type = type'
        }

type StubMeta = {
    Req : StubFieldMeta list
    Evt : StubFieldMeta list
} with
    static member Create req evt : StubMeta =
        {
            Req = req
            Evt = evt
        }