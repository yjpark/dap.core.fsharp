[<AutoOpen>]
module Dap.Platform.BaseAgent

open Dap.Prelude
open Dap.Context
open Dap.Platform

let private tplAgentErr = LogEvent.Template3<string, obj, obj>(LogLevelFatal, "[{Section}] {Err}: {Detail}")
let private tplAgentFailed = LogEvent.Template2WithException<string, obj>(LogLevelError, "[{Section}] {Msg} -> Failed")

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
    let mutable logger : ILogger = env.Logging.GetLogger <| ident.ToLuid ()
    let mutable spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable dispatch : Elmish.Dispatch<'msg> option = None
    let mutable actor : Actor<'args, 'model, 'msg, 'req, 'evt> option = None
    member this.AsDisplay = (ident, this.Actor)
    override this.ToString () = sprintf "<Agent>%s" <| ident.ToString ()
    member this.AsAgent1 = this :> IAgent
    member this.AsAgent2 = this :> IAgent<'req, 'evt>
    member this.AsAgent2' = this :> IAgent<'msg>
    member this.AsAgent3 = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member this.AsAgent = this.AsAgent3
    member __.Setup' spec' =
        if spec.IsSome then
            failWith "Already_Setup" (spec, spec')
        spec <- Some spec'
    member this.Spec = spec |> Option.get
    //IRunner<'runner>
    abstract member Runner : 'runner with get
    interface IRunner<'runner> with
        member this.Runner = this.Runner
    //IRunner
    member __.Clock = env.Clock
    interface IRunner with
        member this.Clock = env.Clock
    member this.Start' () =
        let runner = this :> IAgent<'msg>
        try
            let (model, cmd) =
                match actor with
                | Some actor ->
                    failWith "Already_Started" actor
                | None ->
                    let (model, cmd) = this.Spec.Logic.Init runner this.Spec.Args
                    let actor' = new Actor<'args, 'model, 'msg, 'req, 'evt> (this, this.Spec, model)
                    actor <- Some <| actor'
                    logger <- enrichLoggerForAgent this logger
                    (model, cmd)
            let runner = this.Runner
            try
                batchCmd [
                    cmd
                    this.Spec.Logic.Subscribe runner model
                ]
            with e ->
                runner.Log <| tplAgentFailed "Subscribe" () e
                noCmd
        with e ->
            runner.Log <| tplAgentFailed "Init" () e
            noCmd
    member this.Process' msg =
        let runner = this.Runner
        try
            let (model, cmd) =
                match actor with
                | None ->
                    failWith "Not_Started" msg
                | Some actor ->
                    let (model, cmd) = this.Spec.Logic.Update runner msg actor.State
                    actor.SetState msg model
                    (model, cmd)
            cmd
        with e ->
            runner.Log <| tplAgentFailed "Update" msg e
            noCmd
    member __.SetDispatch' dispatch' : unit =
        dispatch <- Some dispatch'
    member __.Deliver' (cmd : Cmd<'msg>) : unit =
        let dispatch' = Option.get dispatch
        cmd |> List.iter (fun m -> m <| dispatch')
    interface IOwner with
        member __.Luid = ident.ToLuid ()
        member __.Disposed = false
    //IAgent<'args, 'model, 'msg, 'req, 'evt>
    member __.Actor = actor |> Option.get |> fun a -> a.AsActor
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor
        member this.AsAgent2 = this.AsAgent2
        member this.AsAgent2' = this.AsAgent2'
    //IAgent<'req, 'evt>
    member this.Post (subReq : 'req) =
        this.Actor.Handle subReq
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.Actor2 = this.Actor :> IActor<'req, 'evt>
        member this.AsAgent1 = this.AsAgent1
    //IAgent<'msg>
    member this.Deliver (msg : 'msg) = this.Deliver' <| cmdOfMsg msg
    interface IAgent<'msg> with
        member this.Deliver msg = this.Deliver msg
    //IAgent
    member __.Env = env
    member __.Ident = ident
    interface IAgent with
        member this.Env = env
        member __.Ident = ident
    interface ILogger with
        member this.Log m = logger.Log m

[<AbstractClass>]
type PackAgent<'pack, 'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'pack :> IPack
                    and 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
        (pack : 'pack, param) =
    inherit BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt> (param)
    member __.Pack = pack
    interface IPackAgent<'pack> with
        member this.Pack = this.Pack

