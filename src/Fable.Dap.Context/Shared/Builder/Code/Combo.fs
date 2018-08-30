[<RequireQualifiedAccess>]
module Dap.Context.Builder.Code.Combo

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder.Value.Util

type Builder (kind') =
    let kind : Kind = kind'
    let mutable fields : string list = []
    let addField key =
        fields <- addFieldMember key fields
    member this.Yield (_ : 'a) = this.Zero ()
    member __.Zero () : string list =
        [
            sf """type %s (owner : IOwner, key : Key) =""" kind
            sf """    inherit WrapProperties<%s, IComboProperty> ()""" kind
            sf """    let target = Properties.combo owner key"""
        ]|> List.rev
    member __.Run(this : string list) =
        [
            id """    do ("""
            id """        target.SealCombo ()"""
            id """        base.Setup (target)"""
            id """    )"""
            sf """    static member Empty () = new %s (noOwner, NoKey)""" kind
            id """    override this.Self = this"""
            sf """    override __.Spawn o k = new %s (o, k)""" kind
            id """    override __.SyncTo t = target.SyncTo t.Target"""
        ]|> List.rev
        |> fun m -> fields @ m @ this
        |> List.rev

    [<CustomOperation("custom")>]
    member __.Custom (this : string list, key, kind, spec) =
        addField key
        sf """    let %s = target.AddCustom<%s> "%s" %s""" key kind key spec
        :: this
    [<CustomOperation("combo")>]
    member __.Combo (this : string list, key, spec : string) =
        addField key
        sf """    let %s = target.AddCombo "%s" %s""" key key spec
        :: this
    [<CustomOperation("bool")>]
    member __.Bool (this : string list, key, initValue) =
        addField key
        sf """    let %s = target.AddBool "%s" %b""" key key initValue
        :: this
    [<CustomOperation("int")>]
    member __.Int (this : string list, key, initValue) =
        addField key
        sf """    let %s = target.AddInt "%s" %d""" key key initValue
        :: this
#if !FABLE_COMPILER
    [<CustomOperation("long")>]
    member __.Long (this : string list, key, initValue) =
        addField key
        sf """    let %s = target.AddLong "%s" %d""" key key initValue
        :: this
#endif
    [<CustomOperation("string")>]
    member __.String (this: string list, key, initValue) =
        addField key
        sf """    let %s = target.AddString "%s" "%s"\""" key key initValue
        :: this