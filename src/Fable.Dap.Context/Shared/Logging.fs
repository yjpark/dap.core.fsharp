[<AutoOpen>]
module Dap.Context.Logging

open Dap.Prelude

let private tplPropertyDebug = LogEvent.Template4<string, string, IProperty, obj>(LogLevelDebug, "[{Section}] {Info}: {Prop} {Detail}")

let private tplPropertyInfo = LogEvent.Template4<string, string, IProperty, obj>(LogLevelInformation, "[{Section}] {Info}: {Prop} {Detail}")
let private tplPropertyWarn = LogEvent.Template4<string, string, IProperty, obj>(LogLevelWarning, "[{Section}] {Warn}: {Prop} {Detail}")
let private tplPropertyError = LogEvent.Template4<string, string, IProperty, obj>(LogLevelError, "[{Section}] {Err}: {Prop} {Detail}")

let private tplPropertyException = LogEvent.Template4WithException<string, string, IProperty, obj>(LogLevelError, "[{Section}] {Err}: {Prop} {Detail}")

let logPropDebug (prop : IProperty) section info detail : unit =
    prop.Owner.Log <| tplPropertyDebug section info prop detail

let logPropInfo (prop : IProperty) section info detail : unit =
    prop.Owner.Log <| tplPropertyInfo section info prop detail

let logPropWarn (prop : IProperty) section info detail : unit =
    prop.Owner.Log <| tplPropertyWarn section info prop detail

let logPropError (prop : IProperty) section info detail : unit =
    prop.Owner.Log <| tplPropertyError section info prop detail

let logPropException (prop : IProperty) section info detail e : unit =
    prop.Owner.Log <| tplPropertyException section info prop detail e
