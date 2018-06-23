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
type internal Agent<'args, 'model, 'msg, 'req, 'evt> = {
    Spec : ActorSpec<IAgent, 'args, 'model, 'msg, 'req, 'evt>
    Ident' : Ident
    Logger' : ILogger
    mutable Dispatch : Elmish.Dispatch<'msg> option
    mutable State' : 'model option
} with
    member this.Start () =
        let runner = this :> IAgent
        try
            let (model, cmd) =
                match this.State' with
                | None ->
                    let args = this.Spec.NewArgs (this :> IOwner)
                    this.Spec.Logic.Init runner args
                | Some state ->
                    raiseAgentErr "Already_Started" state
            this.State' <- Some model
            Cmd.batch [
                cmd
                this.Spec.Logic.Subscribe runner model
            ]
        with
        | MessageException msg ->
            runner.Log msg
            Cmd.none
        | e ->
            runner.Log <| tplAgentFailed "Init" () e
            Cmd.none
    member this.Process msg =
        let runner = this :> IAgent
        try
            let (model, cmd) =
                match this.State' with
                | None ->
                    raiseAgentErr "Not_Started" msg
                | Some state ->
                    this.Spec.Logic.Update runner state msg
            this.State' <- Some model
            cmd
        with
        | MessageException msg ->
            runner.Log msg
            Cmd.none
        | e ->
            runner.Log <| tplAgentFailed "Update" msg e
            Cmd.none
    member this.Deliver (cmd : Cmd<'msg>) : unit =
        let dispatch = Option.get this.Dispatch
        cmd |> List.iter (fun m -> m <| dispatch)
    member this.SetDispatch (dispatch : Elmish.Dispatch<'msg>) =
        this.Dispatch <- Some dispatch
    member this.Post (subReq : 'req) =
        let dispatch = Option.get this.Dispatch
        dispatch <| this.Spec.WrapReq subReq
    interface IActor<'model, 'req, 'evt> with
        member this.Handle (req : 'req) =
            let dispatch = Option.get this.Dispatch
            dispatch <| this.Spec.WrapReq req
        member this.OnEvent =
            let model = Option.get this.State'
            this.Spec.GetOnEvent model
        member this.Ident = this.Ident'
        member this.State = this.State'
    interface IOwner with
        member this.Ident = this.Ident'.Ident
        member _this.Disposed = false
    interface IAgent with
        member this.Ident = this.Ident'
        member this.Self' = this :> IAgent
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.Actor = this :> IActor<'req, 'evt>
    interface IAgent<'model, 'req, 'evt> with
        member this.Actor = this :> IActor<'model, 'req, 'evt>
    interface ILogger with
        member this.Log m = this.Logger'.Log m