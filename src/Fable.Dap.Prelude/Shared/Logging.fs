[<AutoOpen>]
module Dap.Prelude.Logging

type LogLevel =
    | LogLevelFatal
    | LogLevelError
    | LogLevelWarning
    | LogLevelInformation
    | LogLevelDebug
    | LogLevelVerbose
    member this.ToShortString =
        match this with
        | LogLevelFatal -> "FTL"
        | LogLevelError -> "ERR"
        | LogLevelWarning -> "WRN"
        | LogLevelInformation -> "INF"
        | LogLevelDebug -> "DBG"
        | LogLevelVerbose -> "VRB"
    member this.ToInt : int =
        match this with
        | LogLevelFatal -> 5
        | LogLevelError -> 4
        | LogLevelWarning -> 3
        | LogLevelInformation -> 2
        | LogLevelDebug -> 1
        | LogLevelVerbose -> 0

type LogEvent = {
    Level : LogLevel
    Format : string
    Params : obj list
    Exception : exn option
}

type ILogger =
    abstract Log : LogEvent -> unit

type ILogging =
    inherit ILogger
    abstract Close : unit -> unit
    abstract GetLogger : string -> ILogger

type FallbackLogger (prefix : string) =
    member __.Prefix = prefix
    member this.Log (evt : LogEvent) =
        printfn "[%s] %s%s" evt.Level.ToShortString this.Prefix evt.Format
        evt.Params
        |> List.iter (fun p -> printfn "\t%A" p)
        evt.Exception
        |> Option.map (fun e -> printfn "Exception: %s\nStackTrace: %s" e.Message e.StackTrace)
        |> ignore
    with
        interface ILogger with
            member this.Log evt = this.Log evt

type FallBackLogging () =
    member __.Logger = FallbackLogger ""
    with
        interface ILogging with
            member __.Close () = ()
            member __.GetLogger (context : string) =
                FallbackLogger (sprintf "<%s> " context)
                :> ILogger
        interface ILogger with
            member this.Log evt = this.Logger.Log evt

let mutable private ``_Logging`` = FallBackLogging () :> ILogging

let getLogger (context : string) =
    _Logging.GetLogger context

let getLogging () = _Logging

let setLogging'<'logging when 'logging :> ILogging> (logging : 'logging) =
    _Logging <- logging
    logging

let private checkTemplateParamCount (_format : string) (_count : int) : unit =
    //TODO
    ()
    (*
    let template' = parse template
    if  template'.IsAllPositional || template'.Properties.Length <> count then
        raise (System.ArgumentException (sprintf "Invalid Template: '%s' %d <> %d" template count template'.Properties.Length))
     *)

//Note: for these Template methods to work on fable, need to have different names
//  also it's cleaner, so keep it this way.
type LogEvent with
    static member Template1<'T1> (level : LogLevel, format : string) : 'T1 -> LogEvent =
        checkTemplateParamCount format 1
        fun (p1 : 'T1) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ]
                Exception = None
            }
    static member Template2<'T1, 'T2> (level : LogLevel, format : string) : 'T1 -> 'T2 -> LogEvent =
        checkTemplateParamCount format 2
        fun (p1 : 'T1) (p2 : 'T2) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ]
                Exception = None
            }
    static member Template3<'T1, 'T2, 'T3> (level : LogLevel, format : string) : 'T1 -> 'T2 -> 'T3 -> LogEvent =
        checkTemplateParamCount format 3
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ]
                Exception = None
            }

    static member Template4<'T1, 'T2, 'T3, 'T4> (level : LogLevel, format : string)
                                                : 'T1 -> 'T2 -> 'T3 -> 'T4 -> LogEvent =
        checkTemplateParamCount format 4
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) (p4 : 'T4) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ; box p4 ]
                Exception = None
            }
    static member Template5<'T1, 'T2, 'T3, 'T4, 'T5> (level : LogLevel, format : string)
                                                : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> LogEvent =
        checkTemplateParamCount format 5
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) (p4 : 'T4) (p5 : 'T5) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ; box p4 ; box p5 ]
                Exception = None
            }
    static member Template6<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> (level : LogLevel, format : string)
                                                : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> LogEvent =
        checkTemplateParamCount format 6
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) (p4 : 'T4) (p5 : 'T5) (p6 : 'T6)->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ; box p4 ; box p5 ; box p6 ]
                Exception = None
            }
    static member Template1WithException<'T1> (level : LogLevel, format : string) : 'T1 -> exn -> LogEvent =
        checkTemplateParamCount format 1
        fun (p1 : 'T1) (e : exn) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ]
                Exception = Some e
            }
    static member Template2WithException<'T1, 'T2> (level : LogLevel, format : string) : 'T1 -> 'T2 -> exn -> LogEvent =
        checkTemplateParamCount format 2
        fun (p1 : 'T1) (p2 : 'T2) (e : exn) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ]
                Exception = Some e
            }
    static member Template3WithException<'T1, 'T2, 'T3> (level : LogLevel, format : string) : 'T1 -> 'T2 -> 'T3 -> exn -> LogEvent =
        checkTemplateParamCount format 3
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) (e : exn) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ]
                Exception = Some e
            }

    static member Template4WithException<'T1, 'T2, 'T3, 'T4> (level : LogLevel, format : string)
                                                : 'T1 -> 'T2 -> 'T3 -> 'T4 -> exn -> LogEvent =
        checkTemplateParamCount format 4
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) (p4 : 'T4) (e : exn) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ; box p4 ]
                Exception = Some e
            }
    static member Template5WithException<'T1, 'T2, 'T3, 'T4, 'T5> (level : LogLevel, format : string)
                                                : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> exn -> LogEvent =
        checkTemplateParamCount format 5
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) (p4 : 'T4) (p5 : 'T5) (e : exn) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ; box p4 ; box p5 ]
                Exception = Some e
            }

    static member Template6WithException<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> (level : LogLevel, format : string)
                                                : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> exn -> LogEvent =
        checkTemplateParamCount format 6
        fun (p1 : 'T1) (p2 : 'T2) (p3 : 'T3) (p4 : 'T4) (p5 : 'T5) (p6 : 'T6) (e : exn) ->
            {
                Level = level
                Format = format
                Params = [ box p1 ; box p2 ; box p3 ; box p4 ; box p5 ; box p6 ]
                Exception = Some e
            }

let private tplInfo = LogEvent.Template3<string, string, obj>(LogLevelInformation, "[{Section}] {Info}: {Detail}")

let private tplWarn = LogEvent.Template3<string, string, obj>(LogLevelWarning, "[{Section}] {Warn}: {Detail}")
let private tplError = LogEvent.Template3<string, string, obj>(LogLevelError, "[{Section}] {Err}: {Detail}")

let private tplException = LogEvent.Template3WithException<string, string, obj>(LogLevelError, "[{Section}] {Err}: {Detail}")

let log (evt : LogEvent) (logger : ILogger) =
    logger.Log evt
    logger

let failWith err detail =
    failwithf "%s: %A" err detail

let logInfo (logger : ILogger) section info detail : unit =
    logger.Log <| tplInfo section info detail

let logWarn (logger : ILogger) section info detail : unit =
    logger.Log <| tplWarn section info detail

let logError (logger : ILogger) section err detail : unit =
    logger.Log <| tplError section err detail

let logException (logger : ILogger) section err detail e : unit =
    logger.Log <| tplException section err detail e

let logWip (logger : ILogger) err detail =
    logError logger "WIP" err detail