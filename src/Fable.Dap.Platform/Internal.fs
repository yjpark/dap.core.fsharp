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
type internal Agent<'args, 'model, 'msg, 'req, 'evt>
                        when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt>
    Ident' : Ident
    mutable Logger' : ILogger
    mutable Dispatch : Elmish.Dispatch<'msg> option
    mutable State' : 'model option
    mutable Actor' : Actor<'args, 'model, 'msg, 'req, 'evt> option
    mutable Version' : ActorVersion
} with
    member this.AsDisplay = (this.Ident', this.Version')
    member this.AsAgent =
        this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member this.Actor =
        if this.Actor'.IsNone then
            this.Actor' <- Some <| Actor<'args, 'model, 'msg, 'req, 'evt>.Create this
        this.Actor'
        |> Option.get
    member this.SetState state =
        if this.State'.IsSome && (state =? Option.get this.State') then
            this.Version' <-
                {this.Version' with
                    MsgCount = this.Version'.MsgCount + 1
                }
        else
            this.Version' <-
                {this.Version' with
                    StateVer = this.Version'.StateVer + 1
                    MsgCount = this.Version'.MsgCount + 1
                }
            this.State' <- Some state
    member this.Start () =
        let runner = this :> IAgent<'msg, 'req, 'evt>
        try
            let (model, cmd) =
                match this.State' with
                | None ->
                    this.Logger' <- enrichLoggerForAgent this this.Logger'
                    let args = this.Actor.Args'
                    this.Spec.Logic.Init runner args
                | Some state ->
                    raiseAgentErr "Already_Started" state
            this.SetState model
            let runner = this.AsAgent
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
    member this.Process msg =
        let runner = this.AsAgent
        try
            let (model, cmd) =
                match this.State' with
                | None ->
                    raiseAgentErr "Not_Started" msg
                | Some state ->
                    this.Spec.Logic.Update runner state msg
            this.SetState model
            match this.Spec.CastEvt msg with
            | Some evt ->
                this.Actor.FireEvent' evt
            | None ->
                ()
            cmd
        with
        | MessageException msg ->
            runner.Log msg
            noCmd
        | e ->
            runner.Log <| tplAgentFailed "Update" msg e
            noCmd
    member this.Deliver (cmd : Cmd<'msg>) : unit =
        let dispatch = Option.get this.Dispatch
        cmd |> List.iter (fun m -> m <| dispatch)
    member this.SetDispatch (dispatch : Elmish.Dispatch<'msg>) =
        this.Dispatch <- Some dispatch
    member this.Post (subReq : 'req) =
        let dispatch = Option.get this.Dispatch
        dispatch <| this.Spec.WrapReq subReq
    interface IOwner with
        member this.Ident = this.Ident'.Ident
        member _this.Disposed = false
    interface IAgent with
        member this.Ident = this.Ident'
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.Actor = this.Actor :> IActor<'req, 'evt>
    interface IAgent<'msg, 'req, 'evt> with
        member this.Deliver' (cmd : Cmd<'msg>) = this.Deliver cmd
        member this.Deliver (msg : 'msg) = this.Deliver <| Cmd.ofMsg msg
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor :> IActor<'args, 'model, 'req, 'evt>
    interface ILogger with
        member this.Log m = this.Logger'.Log m

and [<StructuredFormatDisplay("<Actor>{Ident}")>]
    internal Actor<'args, 'model, 'msg, 'req, 'evt>
                        when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Agent : Agent<'args, 'model, 'msg, 'req, 'evt>
    //Can NOT use same name in Actor implementation in Fable 1.x
    //https://github.com/fable-compiler/Fable/issues/1343
    Args' : 'args
    OnEvent' : IBus<'evt>
    FireEvent' : 'evt -> unit
} with
    static member Create agent =
        let args = agent.Spec.NewArgs (agent :> IAgent)
        let event = new Bus<'evt> (agent :> IOwner)
        {
            Agent = agent
            Args' = args
            OnEvent' = event.Publish
            FireEvent' = event.Trigger
        }
    member this.Ident = this.Agent.Ident'
    interface IActor<'args, 'model, 'req, 'evt> with
        member this.Handle req = this.Agent.Post req
        member this.OnEvent = this.OnEvent'
        member this.Ident = this.Agent.Ident'
        member this.Args = this.Args'
        member this.State =
            this.Agent.State'
            |> Option.get
        member this.Version =
            this.Agent.Version'
