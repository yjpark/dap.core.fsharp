[<AutoOpen>]
module Dap.Context.Logging

open Dap.Prelude

let private tplAspectDebug = LogEvent.Template4<string, string, IAspect, obj>(LogLevelDebug, "[{Section}] {Info}: {Aspect} {Detail}")

let private tplAspectInfo = LogEvent.Template4<string, string, IAspect, obj>(LogLevelInformation, "[{Section}] {Info}: {Aspect} {Detail}")
let private tplAspectWarn = LogEvent.Template4<string, string, IAspect, obj>(LogLevelWarning, "[{Section}] {Warn}: {Aspect} {Detail}")
let private tplAspectError = LogEvent.Template4<string, string, IAspect, obj>(LogLevelError, "[{Section}] {Err}: {Aspect} {Detail}")

let private tplAspectException = LogEvent.Template4WithException<string, string, IAspect, obj>(LogLevelError, "[{Section}] {Err}: {Aspect} {Detail}")

let logAspectDebug (aspect : IAspect) info detail : unit =
    aspect.Owner.Log <| tplAspectDebug aspect.SpecA.Luid info aspect detail

let logAspectInfo (aspect : IAspect) info detail : unit =
    aspect.Owner.Log <| tplAspectInfo aspect.SpecA.Luid info aspect detail

let logAspectWarn (aspect : IAspect) info detail : unit =
    aspect.Owner.Log <| tplAspectWarn aspect.SpecA.Luid info aspect detail

let logAspectError (aspect : IAspect) info detail : unit =
    aspect.Owner.Log <| tplAspectError aspect.SpecA.Luid info aspect detail

let logAspectException (aspect : IAspect) info detail e : unit =
    aspect.Owner.Log <| tplAspectException aspect.SpecA.Luid info aspect detail e
