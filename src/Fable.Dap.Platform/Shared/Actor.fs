[<AutoOpen>]
module Dap.Platform.Actor

//Note: during Init, the model is not created yet
type ActorInit<'args, 'model, 'msg, 'req, 'evt> =
    Init<IAgent<'req, 'evt>, 'args, 'model, 'msg>

/// Change model according to msg, also may generate cmds.
type ActorUpdate<'model, 'msg, 'req, 'evt> =
    Update<IAgent<'model, 'req, 'evt>, 'model, 'msg>

/// Generate msg from outside. e.g. an timer, or keyboard.
type ActorSubscribe<'model, 'msg, 'req, 'evt> =
    Subscribe<IAgent<'req, 'evt>, 'model, 'msg>

type ActorLogic<'args, 'model, 'msg, 'req, 'evt> =
    Logic<IAgent<'req, 'evt>, IAgent<'model, 'req, 'evt>, 'args, 'model, 'msg>

type ActorSpec<'args, 'model, 'msg, 'req, 'evt> =
    ActorSpec'<IAgent<'req, 'evt>, IAgent<'model, 'req, 'evt>, 'args, 'model, 'msg, 'req, 'evt>

type ActorOperate<'model, 'msg, 'req, 'evt> =
    Operate<IAgent<'model, 'req, 'evt>, 'model, 'msg>