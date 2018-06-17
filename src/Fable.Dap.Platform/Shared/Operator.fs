[<AutoOpen>]
module Dap.Platform.Operator

let (|-|-) (first : Operate<'runner, 'model, 'msg>) (second : Operate<'runner, 'model, 'msg>) : Operate<'runner, 'model, 'msg> =
    fun runner (model, cmd) ->
        second runner <| first runner (model, cmd)

let (|-|>) ((runner, model, cmd) : 'runner * 'model * Cmd<'msg>) (op : Operate<'runner, 'model, 'msg>)
                : 'runner * 'model * Cmd<'msg> =
    let (model, cmd) = op runner (model, cmd)
    (runner, model, cmd)

let (|=|>) ((runner, model, cmd) : 'runner * 'model * Cmd<'msg>) (op : Operate<'runner, 'model, 'msg>)
                : 'model * Cmd<'msg> =
    op runner (model, cmd)