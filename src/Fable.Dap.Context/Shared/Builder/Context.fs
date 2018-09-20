[<RequireQualifiedAccess>]
module Dap.Context.Builder.Context

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Helper

type Builder (kind : Kind) =
    inherit ObjBuilder<IContext> ()
    override __.Zero () =
        IContext.Default kind

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
        | :? IDictProperty as properties ->
#if !FABLE_COMPILER
            newContext "map0" properties.ElementType properties.ElementSpawner
#else
            //TODO
            context
#endif
        | :? IListProperty as properties ->
#if !FABLE_COMPILER
            newContext "list0" properties.ElementType properties.ElementSpawner
#else
            //TODO
            context
#endif
        | :? ICustomProperties as properties ->
            newContext "custom0" propertiesType properties.Clone0
        | _ ->
            logError "Not_Support"
            context
