module Dap.Platform.Internal

open Elmish
open Dap.Prelude
open Dap.Platform

let private tplActorErr = LogEvent.Template3<string, obj, obj>(LogLevelFatal, "[{Section}] {Err}: {Detail}")

let private raiseActorErr err detail = 
    raiseWith <| tplActorErr "Actor" err detail

//Can NOT use same name in Actor implementation in Fable 1.x
//https://github.com/fable-compiler/Fable/issues/1343
type internal Actor<'args, 'model, 'msg, 'req, 'evt> = {
    Spec : ActorSpec<IActor, 'args, 'model, 'msg, 'req, 'evt>
    Ident' : Ident
    Logger' : ILogger
    mutable Dispatch : Elmish.Dispatch<'msg> option
    mutable State' : 'model option
} with
    member this.Start () =
        let runner = this :> IActor
        let (model, cmd) =
            match this.State' with
            | None ->
                let args = this.Spec.NewArgs ()
                this.Spec.Logic.Init runner args
            | Some state ->
                raiseActorErr "Already_Started" state
        this.State' <- Some model
        Cmd.batch [
            cmd
            this.Spec.Logic.Subscribe runner model
        ]
    member this.Process msg =
        let runner = this :> IActor
        let (model, cmd) =
            match this.State' with
            | None ->
                raiseActorErr "Not_Started" msg
            | Some state ->
                this.Spec.Logic.Update runner state msg
        this.State' <- Some model
        cmd
    member this.Deliver (cmd : Cmd<'msg>) : unit =
        let dispatch = Option.get this.Dispatch
        cmd |> List.iter (fun m -> m <| dispatch)
    member this.SetDispatch (dispatch : Elmish.Dispatch<'msg>) =
        this.Dispatch <- Some dispatch
    interface IActor<'model, 'req, 'evt> with
        member this.Handle (req : 'req) =
            let dispatch = Option.get this.Dispatch
            dispatch <| this.Spec.WrapReq req
        member this.OnEvent =
            let model = Option.get this.State'
            this.Spec.GetOnEvent model
        member this.Ident = this.Ident'
        member this.State = this.State'
    interface IPoster<'msg> with
        member this.Post (msg : 'msg) =
            let dispatch = Option.get this.Dispatch
            dispatch msg
    interface ILogger with
        member this.Log m = this.Logger'.Log m