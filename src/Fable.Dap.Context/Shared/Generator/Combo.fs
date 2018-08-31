[<RequireQualifiedAccess>]
module Dap.Context.Generator.Combo

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal
open Dap.Context.Generator.Util

type Field =
    | VarField of IPropertySpec
    | CustomField of Kind * Key

and Generator (template : IComboProperty) =
    let getClassHeader kind =
        [
            sf """type %s (owner : IOwner, key : Key) =""" kind
            sf """    inherit WrapProperties<%s, IComboProperty> ("%s")""" kind kind
            sf """    let target = Properties.combo owner key"""
        ]
    let getClassMiddle kind =
        [
            id """    do ("""
            id """        target.SealCombo ()"""
            id """        base.Setup (target)"""
            id """    )"""
            sf """    static member Create o k = new %s (o, k)""" kind
            sf """    static member Empty () = %s.Create noOwner NoKey""" kind
            id """    override this.Self = this"""
            sf """    override __.Spawn o k = %s.Create o k""" kind
            id """    override __.SyncTo t = target.SyncTo t.Target"""
        ]
    let getFieldAdder (prop : IProperty) =
        let spec = prop.Spec
        sf """    let %s = target.Add%s "%s" %s""" spec.Key spec.Kind spec.Key (E.encode 0 spec.InitValue)
    let getFieldMember (prop : IProperty) =
        let spec = prop.Spec
        sf """    member __.%s = %s""" (toCamelCase spec.Key) spec.Key
    interface IGenerator with
        member __.GenerateClass kind =
            let fields =
                template.Value
                |> Map.toList
                |> List.map snd
            [
                getClassHeader kind
                fields |> List.map getFieldAdder
                getClassMiddle kind
                fields |> List.map getFieldMember
            ]|> List.reduce (@)
        member __.GenerateBuilder kind = []