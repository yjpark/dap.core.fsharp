module Dap.Context.Builder

open System.Reflection

open Dap.Prelude
open Dap.Context

[<AbstractClass>]
type ObjBuilder<'obj when 'obj :> IObj> () =
    member this.Yield (_ : 'a) =
        this.Zero ()
    abstract member Zero : unit -> 'obj

type ComboBuilder () =
    inherit ObjBuilder<IComboProperty> ()
    override __.Zero () =
        IComboProperty.Empty noOwner
    [<CustomOperation("custom")>]
    member __.Custom (this : IComboProperty, key, prop : ICustomProperty) =
        this.AddAny key prop.Clone0 |> ignore
        this
    [<CustomOperation("combo")>]
    member __.Combo (this : IComboProperty, key, prop : IComboProperty) =
        this.AddAny key prop.Clone0 |> ignore
        this
    [<CustomOperation("bool")>]
    member __.Bool (this : IComboProperty, key, initValue, validator) =
        this.AddBool key initValue validator |> ignore
        this
    [<CustomOperation("int")>]
    member __.Int (this : IComboProperty, key, initValue, validator) =
        this.AddInt key initValue validator |> ignore
        this
#if !FABLE_COMPILER
    [<CustomOperation("long")>]
    member __.Long (this : IComboProperty, key, initValue, validator) =
        this.AddLong key initValue validator |> ignore
        this
#endif
    [<CustomOperation("string")>]
    member __.String (this: IComboProperty, key, initValue, validator) =
        this.AddString key initValue validator |> ignore
        this

type ContextBuilder (kind') =
    inherit ObjBuilder<IContext> ()
    let kind : Kind = kind'
    override __.Zero () =
        IContext.Empty kind

    [<CustomOperation("properties")>]
    member __.Properties (context: IContext, properties : IProperties) =
        let propertiesType = properties.GetType()
        let logError tip =
            let err = sprintf "<%s> %s" propertiesType.FullName tip
            logError context "ContextBuilder.Properties" err (E.encodeJson 4 properties)
        let syncProperties = fun (syncTo : 'p -> unit) (context : IContext<'p>) ->
            syncTo context.Properties
            context :> IContext
        let newContext = fun (method : string) (paramType : System.Type) (spawner : PropertySpawner) ->
            typeof<ComboContext>.DeclaringType.GetMethod method
            |> fun x -> x.MakeGenericMethod [| paramType |]
            |> fun x ->
                x.Invoke (null, [| kind ; spawner |])
                :?> IContext
        match properties with
        | :? IComboProperty as properties ->
            Context.combo kind
            |> syncProperties properties.SyncTo
        | :? IMapProperty as properties ->
            newContext "map0" properties.ElementType properties.ElementSpawner
        | :? IListProperty as properties ->
            newContext "list0" properties.ElementType properties.ElementSpawner
        | :? ICustomProperties as properties ->
            newContext "custom0" propertiesType properties.Clone0
        | _ ->
            logError "Not_Support"
            context

let combo = new ComboBuilder ()

let context kind = new ContextBuilder (kind)
