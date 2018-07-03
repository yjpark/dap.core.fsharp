[<AutoOpen>]
module Dap.Platform.Actor

type ActorNewArgs<'args> = NewArgs<IAgent, 'args>

//Note: during Init, the model is not created yet
type ActorInit<'args, 'model, 'msg> when 'msg :> IMsg =
    Init<IAgent<'msg>, 'args, 'model, 'msg>

/// Change model according to msg, also may generate cmds.
type ActorUpdate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

/// Generate msg from outside. e.g. an timer, or keyboard.
type ActorSubscribe<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Subscribe<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type ActorLogic<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Logic<IAgent<'msg>, IAgent<'args, 'model, 'msg, 'req, 'evt>, 'args, 'model, 'msg>

type ActorSpec<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    ActorSpec'<IAgent, IAgent<'msg>, IAgent<'args, 'model, 'msg, 'req, 'evt>, 'args, 'model, 'msg, 'req, 'evt>

type ActorOperate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>
