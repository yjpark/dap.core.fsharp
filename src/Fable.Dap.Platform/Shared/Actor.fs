[<AutoOpen>]
module Dap.Platform.Actor

type ActorNewArgs<'args> = NewArgs<IAgent, 'args>

//Note: during Init, the model is not created yet
type ActorInit<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Init<IAgent<'req, 'evt>, 'args, 'model, 'msg>

/// Change model according to msg, also may generate cmds.
type ActorUpdate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

/// Generate msg from outside. e.g. an timer, or keyboard.
type ActorSubscribe<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Subscribe<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type ActorLogic<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Logic<IAgent<'req, 'evt>, IAgent<'args, 'model, 'msg, 'req, 'evt>, 'args, 'model, 'msg>

type ActorSpec<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    ActorSpec'<IAgent, IAgent<'req, 'evt>, IAgent<'args, 'model, 'msg, 'req, 'evt>, 'args, 'model, 'msg, 'req, 'evt>

type ActorOperate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type IMod<'args, 'model> =
    abstract Args : 'args with get
    abstract State : 'model with get

type IModRunner<'args, 'model, 'msg> =
    inherit IAgent
    abstract Mod : IMod<'args, 'model> with get
    abstract Deliver : 'msg -> unit
#if !FABLE_COMPILER
    abstract RunFunc4<'res> : Func<IModRunner<'args, 'model, 'msg>, 'res> -> Result<'res, exn>
    abstract AddTask4 : OnFailed<IModRunner<'args, 'model, 'msg>> -> GetTask<IModRunner<'args, 'model, 'msg>, unit> -> unit
#endif

type ModInit<'args, 'model, 'msg> =
    Init<IAgent, 'args, 'model, 'msg>

type ModUpdate<'args, 'model, 'msg> =
    Update<IModRunner<'args, 'model, 'msg>, 'model, 'msg>

type ModSpec<'args, 'model, 'msg> = {
    Init : ModInit<'args, 'model, 'msg>
    Update : ModUpdate<'args, 'model, 'msg>
}

type ModOperate<'args, 'model, 'msg> =
    Operate<IModRunner<'args, 'model, 'msg>, 'model, 'msg>

type ModWrapping<'args, 'model, 'msg> =
    IWrapping<IModRunner<'args, 'model, 'msg>, 'model, 'msg>

type ModWrapperSpec<'runner, 'model, 'msg, 'subArgs, 'subModel, 'subMsg> = {
    GetSub : 'model -> 'subModel
    SetSub : 'subModel -> 'model -> 'model
    UpdateSub : ModUpdate<'subArgs, 'subModel, 'subMsg>
    ReactSub : React<'runner, 'model, 'msg, 'subModel, 'subMsg>
}