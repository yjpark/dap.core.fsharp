[<AutoOpen>]
module Dap.Platform.Operator

let (|-|>) (first : Operate<'runner, 'model, 'msg>) (second : Operate<'runner, 'model, 'msg>) : Operate<'runner, 'model, 'msg> =
    fun runner (model, cmd) ->
        second runner <| first runner (model, cmd)