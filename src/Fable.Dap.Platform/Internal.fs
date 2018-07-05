module Dap.Platform.Internal

open Elmish
open Dap.Prelude
open Dap.Platform

let private tplAgentErr = LogEvent.Template3<string, obj, obj>(LogLevelFatal, "[{Section}] {Err}: {Detail}")
let private tplAgentFailed = LogEvent.Template2WithException<string, obj>(LogLevelError, "[{Section}] {Msg} -> Failed")

let private raiseAgentErr err detail =
    raiseWith <| tplAgentErr "Actor" err detail

//Can NOT use same name in Actor implementation in Fable 1.x
//https://github.com/fable-compiler/Fable/issues/1343
//TODO: this is not needed now, since Actor<> been created now
//Cleanup this when got time
[<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
type internal Agent<'args, 'model, 'msg, 'req, 'evt when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt> (spec', ident', logger') =
    let spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt> = spec'
    let ident : Ident = ident'
    let mutable logger : ILogger = logger'
    let mutable dispatch : Elmish.Dispatch<'msg> option = None
    let mutable actor : Actor<'args, 'model, 'msg, 'req, 'evt> option = None
    member _this.AsDisplay = (ident, actor)
    member this.AsAgent =
        this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member _this.Actor = actor |> Option.get
    member this.Start () =
        let runner = this :> IAgent<'msg>
        try
            let (model, cmd) =
                match actor with
                | Some actor ->
                    raiseAgentErr "Already_Started" actor
                | None ->
                    logger <- enrichLoggerForAgent this logger
                    let args : 'args = spec.NewArgs (this :> IOwner)
                    let (model, cmd) = spec.Logic.Init runner args
                    actor <- Some <| new Actor<'args, 'model, 'msg, 'req, 'evt> (this, spec, args, model)
                    (model, cmd)
            let runner = this.AsAgent
            try
                Cmd.batch [
                    cmd
                    spec.Logic.Subscribe runner model
                ]
            with
            | e ->
                runner.Log <| tplAgentFailed "Subscribe" () e
                noCmd
        with
        | MessageException msg ->
            runner.Log msg
            noCmd
        | e ->
            runner.Log <| tplAgentFailed "Init" () e
            noCmd
    member this.Process msg =
        let runner = this.AsAgent
        try
            let (model, cmd) =
                match actor with
                | None ->
                    raiseAgentErr "Not_Started" msg
                | Some actor ->
                    let (model, cmd) = spec.Logic.Update runner actor.State msg
                    actor.SetState msg model
                    (model, cmd)
            cmd
        with
        | MessageException msg ->
            runner.Log msg
            noCmd
        | e ->
            runner.Log <| tplAgentFailed "Update" msg e
            noCmd
    member this.Deliver (cmd : Cmd<'msg>) : unit =
        let dispatch' = Option.get dispatch
        cmd |> List.iter (fun m -> m <| dispatch')
    member this.SetDispatch (dispatch' : Elmish.Dispatch<'msg>) =
        dispatch <- Some dispatch'
    member this.Post (subReq : 'req) =
        this.Actor.Handle subReq
    interface IOwner with
        member _this.Ident = ident.Ident
        member _this.Disposed = false
    interface IAgent with
        member _this.Ident = ident
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.Actor = this.Actor :> IActor<'req, 'evt>
    interface IAgent<'msg> with
        member this.Deliver (msg : 'msg) = this.Deliver <| Cmd.ofMsg msg
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor :> IActor<'args, 'model, 'req, 'evt>
    interface ILogger with
        member this.Log m = logger.Log m
