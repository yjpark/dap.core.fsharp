[<AutoOpen>]
module Dap.Platform.BaseAgent

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
[<AbstractClass>]
type BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
        (param) =
    let env : IEnv = param.Env
    let ident : Ident = Ident.Create param.Env.Scope param.Kind param.Key
    let mutable logger : ILogger = env.Logging.GetLogger ident.Ident
    let mutable spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable dispatch : Elmish.Dispatch<'msg> option = None
    let mutable actor : Actor<'args, 'model, 'msg, 'req, 'evt> option = None
    member this.AsDisplay = (ident, this.Actor)
    member this.AsAgent =
        this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member _this.Setup' spec' =
        if spec.IsSome then
            raiseWithError "BaseAgent" "Already_Setup" (spec, spec')
        spec <- Some spec'
    member this.Spec = spec |> Option.get
    //IRunner<'runner>
    abstract member Runner : 'runner with get
    interface IRunner<'runner> with
        member this.Runner = this.Runner
    //IRunner
    member _this.Clock = env.Clock
    interface IRunner with
        member this.Clock = env.Clock
    member this.Start' () =
        let runner = this :> IAgent<'msg>
        try
            let (model, cmd) =
                match actor with
                | Some actor ->
                    raiseAgentErr "Already_Started" actor
                | None ->
                    let (model, cmd) = this.Spec.Logic.Init runner this.Spec.Args
                    let actor' = new Actor<'args, 'model, 'msg, 'req, 'evt> (this, this.Spec, model)
                    actor <- Some <| actor'
                    logger <- enrichLoggerForAgent this logger
                    (model, cmd)
            let runner = this.Runner
            try
                Cmd.batch [
                    cmd
                    this.Spec.Logic.Subscribe runner model
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
    member this.Process' msg =
        let runner = this.Runner
        try
            let (model, cmd) =
                match actor with
                | None ->
                    raiseAgentErr "Not_Started" msg
                | Some actor ->
                    let (model, cmd) = this.Spec.Logic.Update runner actor.State msg
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
    member _this.SetDispatch' dispatch' : unit =
        dispatch <- Some dispatch'
    member _this.Deliver' (cmd : Cmd<'msg>) : unit =
        let dispatch' = Option.get dispatch
        cmd |> List.iter (fun m -> m <| dispatch')
    interface IOwner with
        member _this.Ident = ident.Ident
        member _this.Disposed = false
    //IAgent<'args, 'model, 'msg, 'req, 'evt>
    member _this.Actor = actor |> Option.get |> fun a -> a.AsActor
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor
    //IAgent<'req, 'evt>
    member this.Post (subReq : 'req) =
        this.Actor.Handle subReq
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.Actor = this.Actor :> IActor<'req, 'evt>
    //IAgent<'msg>
    member this.Deliver (msg : 'msg) = this.Deliver' <| Cmd.ofMsg msg
    interface IAgent<'msg> with
        member this.Deliver msg = this.Deliver msg
    //IAgent
    member _this.Env = env
    member _this.Ident = ident
    interface IAgent with
        member this.Env = env
        member _this.Ident = ident
    interface ILogger with
        member this.Log m = logger.Log m
