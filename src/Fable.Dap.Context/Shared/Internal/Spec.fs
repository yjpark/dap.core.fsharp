[<AutoOpen>]
module Dap.Context.Internal.Spec

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context

type internal AspectSpec = {
    Luid : Luid
    Key : Key
} with
    static member Create luid key =
        {
            Luid = luid
            Key = key
        }
    static member CalcSubLuid (parentLuid : Luid) (subKey : Key) =
        sprintf "%s.%s" parentLuid subKey
    member this.GetSubSpec subKey =
        {this with
            Luid = AspectSpec.CalcSubLuid this.Luid subKey
            Key = subKey
        }
    member this.AsSubSpec (parent : IAspectSpec) =
        {this with
            Luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        }
    member this.ForClone key = AspectSpec.Create key key
    member this.AsSpec = this :> IAspectSpec
    interface IAspectSpec with
        member this.Luid = this.Luid
        member this.Key = this.Key

type internal PropertySpec = {
    Aspect : AspectSpec
    InitValue : Json
} with
    static member Create luid key initValue =
        {
            InitValue = initValue
            Aspect = AspectSpec.Create luid key
        }
    static member GetSubSpec' (this : IPropertySpec) subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        PropertySpec.Create luid subKey this.InitValue
    static member AsSubSpec' (this : IPropertySpec) (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        PropertySpec.Create luid this.Key this.InitValue
    member this.GetSubSpec subKey = {this with Aspect = this.Aspect.GetSubSpec subKey}
    member this.AsSubSpec parent = {this with Aspect = this.Aspect.AsSubSpec parent}
    member this.ForClone key = {this with Aspect = this.Aspect.ForClone key}
    member this.AsSpec = this :> IPropertySpec
    member this.AsSpec0 = this :> IAspectSpec
    member this.Luid = this.Aspect.Luid
    interface IPropertySpec with
        member this.Luid = this.Aspect.Luid
        member this.Key = this.Aspect.Key
        member this.InitValue = this.InitValue

type internal PropertySpec<'v> = {
    Encoder : JsonEncoder<'v>
    Decoder : JsonDecoder<'v>
    Aspect : AspectSpec
    InitValue : 'v
    Validator : Validator<'v> option
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create (encoder : JsonEncoder<'v>)
                        (decoder : JsonDecoder<'v>)
                        (luid : Luid) (key : Key)
                        (initValue : 'v)
                        (validator : Validator<'v> option)
                            : PropertySpec<'v> =
        {
            Encoder = encoder
            Decoder = decoder
            Aspect = AspectSpec.Create luid key
            InitValue = initValue
            Validator = validator
        }
    static member GetSubSpec' (this : IPropertySpec<'v>) subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        PropertySpec<'v>.Create this.Encoder this.Decoder luid subKey this.InitValue this.Validator
    static member AsSubSpec' (this : IPropertySpec<'v>) (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        PropertySpec<'v>.Create this.Encoder this.Decoder luid this.Key this.InitValue this.Validator
    member this.GetSubSpec subKey = {this with Aspect = this.Aspect.GetSubSpec subKey}
    member this.AsSubSpec parent = {this with Aspect = this.Aspect.AsSubSpec parent}
    member this.ForClone key = {this with Aspect = this.Aspect.ForClone key}
    member this.AsSpec = this :> IPropertySpec<'v>
    member this.AsSpec1 = this :> IPropertySpec
    member this.AsSpec0 = this :> IAspectSpec
    member this.Luid = this.Aspect.Luid
    interface IPropertySpec<'v> with
        member this.Encoder = this.Encoder
        member this.Decoder = this.Decoder
        member this.Validator = this.Validator
        member this.InitValue = this.InitValue
    interface IPropertySpec with
        member this.Luid = this.Aspect.Luid
        member this.Key = this.Aspect.Key
        member this.InitValue = this.Encoder this.InitValue

type internal ContextSpec = {
    Kind : Kind
    Luid : Luid
    PropertiesSpawner : IOwner -> IProperties
} with
    static member Create kind propertiesSpawner =
        {
            Luid = newLuid kind
            Kind = kind
            PropertiesSpawner = propertiesSpawner
        }
    member this.AsSpec = this :> IContextSpec
    interface IContextSpec with
        member this.Kind = this.Kind
        member this.Luid = this.Luid
        member this.PropertiesSpawner owner = this.PropertiesSpawner owner
