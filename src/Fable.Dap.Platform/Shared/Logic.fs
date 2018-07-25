namespace Dap.Platform

#if !FABLE_COMPILER
open System.Threading
open System.Threading.Tasks
#endif

// Copied From Elmish
type Dispatch<'msg> = 'msg -> unit

/// Subscription - return immediately, but may schedule dispatch of a message at any time
type Sub<'msg> = Dispatch<'msg> -> unit

/// Cmd - container for subscriptions that may produce messages
type Cmd<'msg> = Sub<'msg> list

/// Initialize model and may generate cmds.
type Init<'initer, 'args, 'model, 'msg> =
    'initer -> 'args -> 'model * Cmd<'msg>

/// Change model according to msg, also may generate cmds.
type Update<'runner, 'model, 'msg> =
    'runner -> 'model -> 'msg -> 'model * Cmd<'msg>

/// Generate msg from outside. e.g. an timer, or keyboard.
type Subscribe<'runner, 'model, 'msg> =
    'runner -> 'model -> Cmd<'msg>

/// Abstraction of reactive logic, which have model, and can accept msg
type Logic<'initer, 'runner, 'args, 'model, 'msg> = {
    Init : Init<'initer, 'args, 'model, 'msg>
    Update : Update<'runner, 'model, 'msg>
    Subscribe : Subscribe<'runner, 'model, 'msg>
}

type Operate<'runner, 'model, 'msg> =
    'runner -> 'model * Cmd<'msg> -> 'model * Cmd<'msg>

type IWrapping<'runner, 'model, 'msg> =
    abstract Operate : Operate<'runner, 'model, 'msg> with get

type WrapMsg<'runner, 'model, 'msg> =
    IWrapping<'runner, 'model, 'msg> -> 'msg

type React<'runner, 'model, 'msg, 'subModel, 'subMsg> =
    'runner -> 'subMsg -> 'subModel -> 'model -> 'model * Cmd<'msg>

type WrapperSpec<'runner, 'model, 'msg, 'subModel, 'subMsg> = {
    GetSub : 'model -> 'subModel
    SetSub : 'subModel -> 'model -> 'model
    UpdateSub : Update<'runner, 'subModel, 'subMsg>
    ReactSub : React<'runner, 'model, 'msg, 'subModel, 'subMsg>
}

type Wrapper<'msg, 'subMsg> =
    'subMsg -> 'msg

type Spec<'initer, 'runner, 'model, 'msg, 'subArgs, 'subModel, 'subMsg> = {
    SubArgs : 'subArgs
    SubLogic : Logic<'initer, 'runner, 'subArgs, 'subModel, 'subMsg>
    ReactSub : React<'runner, 'model, 'msg, 'subModel, 'subMsg>
}

type SubUpdate<'runner, 'model, 'msg, 'subMsg> =
    'runner -> 'model -> 'subMsg -> 'model * Cmd<'msg>

type Api<'runner, 'req, 'res> =
    'runner -> 'req -> ('res -> unit) -> unit

#if !FABLE_COMPILER
type AsyncApi<'runner, 'req, 'res> =
    'runner -> 'req -> Task<'res>
#endif