module Dap.Context.Builder.Value.Util

open System.Reflection

open Dap.Prelude
open Dap.Context

let sf = sprintf

let getFieldName (name : string) =
    //TODO with CamelCase
    name.ToUpper ()

let addFieldMember (key : string) fields =
    sf """    member __.%s = %s""" (getFieldName key) key
    :: fields
