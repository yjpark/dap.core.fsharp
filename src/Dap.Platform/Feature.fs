[<AutoOpen>]
module Dap.Platform.Feature

open System
open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe

type IFeature =
    inherit IContext

type IFallback = interface end

let private typeIObj = typeof<IObj>
let private typeILogger = typeof<ILogger>
let private typeIOwner = typeof<IOwner>
let private typeIJson = typeof<IJson>
let private typeIContext = typeof<IContext>
let private typeIUnsafeContext = typeof<IUnsafeContext>
let private typeIFeature = typeof<IFeature>
let private typeIFallback = typeof<IFallback>

let private isFeature (type' : Type) =
    Array.contains typeIFeature <| type'.GetInterfaces ()

let private isFallback (type' : Type) =
    Array.contains typeIFallback <| type'.GetInterfaces ()

let mutable private features : Map<string, Type> option = None

let private getKind (type' : Type) = type'.FullName

let private getFeatureKinds (type' : Type) =
    type'.GetInterfaces ()
    |> Array.filter (fun t ->
        t <> typeIObj
            && t <> typeILogger
            && t <> typeIOwner
            && t <> typeIJson
            && t <> typeIContext
            && t <> typeIUnsafeContext
            && t <> typeIFeature
            && t <> typeIFallback
            && (t.GetGenericArguments ()) .Length = 0
    )|> Array.map getKind

let private addFeature (features : Map<string, Type>) ((kind, type') : string * Type) : Map<string, Type> =
    match Map.tryFind kind features with
    | None ->
        Map.add kind type' features
    | Some oldType ->
        if isFallback oldType then
            Map.add kind type' features
        elif isFallback type' then
            features
        else
            logError (getLogger "addFeature") "Feature_Conflicted" kind (oldType, type')
            Map.add kind type' features

let private loadFeatures () : Map<string, Type> =
    AppDomain.CurrentDomain.GetAssemblies ()
    |> Array.map (fun assembly ->
        assembly.GetTypes ()
        |> Array.filter (fun t ->
            isFeature t
                && not t.IsInterface
                && not t.IsAbstract
        )|> Array.map (fun type' ->
            getFeatureKinds type'
            |> Array.map (fun kind ->
                (kind, type')
            )
        )|> Array.concat
    )|> Array.concat
    |> Array.fold addFeature Map.empty

let getFeatures () =
    if features.IsNone then
        features <- Some <| loadFeatures ()
    features.Value

let addToAgent<'feature when 'feature :> IFeature> (agent : IAgent) : 'feature =
    let kind = getKind typeof<'feature>
    getFeatures ()
    |> Map.tryFind kind
    |> function
        | Some type' ->
            Activator.CreateInstance (type', [| (agent.Env.Logging :> obj) |])
            :?> 'feature
        | None ->
            failWith "Feature.AddToAgent" kind
