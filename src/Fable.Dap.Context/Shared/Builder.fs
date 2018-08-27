[<AutoOpen>]
module Dap.Context.Builder

open Dap.Prelude

type PropertiesBuilder (key') =
    let key : Key = key'
    member __.Yield (_ : 'a) =
        let owner = noOwner
        Properties.create owner key <| E.object []
    member __.Combine(this : 'p when 'p :> IProperties, prop : IProperty) =
        this.Add prop
        this
    (*
    member __.Zero () =
        let owner = noOwner
        Properties.create owner key <| E.object []
    member __.For(this : IProperties, that : IProperties) =
        this.Add that
        this
    *)
    [<CustomOperation("add")>]
    member __.Add(this : 'p when 'p :> IProperties, prop : IProperty) =
        this.Add prop
        this
    [<CustomOperation("bool")>]
    member __.Bool(this : 'p when 'p :> IProperties, key, initValue, validator) =
        this.AddBool key initValue validator |> ignore
        this
    [<CustomOperation("int")>]
    member __.Int(this : 'p when 'p :> IProperties, key, initValue, validator) =
        this.AddInt key initValue validator |> ignore
        this
#if !FABLE_COMPILER
    [<CustomOperation("long")>]
    member __.Long(this : 'p when 'p :> IProperties, key, initValue, validator) =
        this.AddLong key initValue validator |> ignore
        this
#endif
    [<CustomOperation("string")>]
    member __.String(this : 'p when 'p :> IProperties, key, initValue, validator) =
        this.AddString key initValue validator |> ignore
        this

type ContextBuilder (kind') =
    inherit PropertiesBuilder (NoKey)
    let kind : Kind = kind'
    member __.Yield (x : 'a) =
        let logging = getLogging ()
        Context.create logging kind <| E.object []

let properties key = new PropertiesBuilder (key)

let context kind = new ContextBuilder (kind)