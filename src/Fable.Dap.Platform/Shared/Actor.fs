[<AutoOpen>]
module Dap.Platform.Actor

//Note: during Init, the model is not created yet
type ActorInit<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Init<IAgent<'req, 'evt>, 'args, 'model, 'msg>

/// Change model according to msg, also may generate cmds.
type ActorUpdate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IAgent<'args, 'model, 'req, 'evt>, 'model, 'msg>

/// Generate msg from outside. e.g. an timer, or keyboard.
type ActorSubscribe<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Subscribe<IAgent<'args, 'model, 'req, 'evt>, 'model, 'msg>

type ActorLogic<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Logic<IAgent<'req, 'evt>, IAgent<'args, 'model, 'req, 'evt>, 'args, 'model, 'msg>

type ActorSpec<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    ActorSpec'<IAgent<'req, 'evt>, IAgent<'args, 'model, 'req, 'evt>, 'args, 'model, 'msg, 'req, 'evt>

type ActorOperate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IAgent<'args, 'model, 'req, 'evt>, 'model, 'msg>