[<AutoOpen>]
module Dap.Platform.Wrapping

open Elmish

// This warpper logic is mostly following elm-component-updater
// https://github.com/mpdairy/elm-component-updater/blob/master/src/Updater.elm

let rec private makeOperate (wrap : Wrap<'runner, 'model, 'msg>)
                            (spec : WrapperSpec<'runner, 'model, 'msg, 'subModel, 'subMsg>) (subMsg : 'subMsg)
                                : Operate<'runner, 'model, 'msg> =
    fun runner (model, cmd) ->
        let (subModel, subCmd) = spec.UpdateSub runner (spec.GetSub model) subMsg
        let (reactModel, reactCmd) = spec.ReactSub runner subMsg subModel (spec.SetSub subModel model)
        let mapSubCmd = fun (m : 'subMsg) -> wrap <| makeOperate wrap spec m
        let subCmd = Cmd.map mapSubCmd subCmd
        let cmd = Cmd.batch [cmd; subCmd; reactCmd]
        (reactModel, cmd)

let wrap (wrap : Wrap<'runner, 'model, 'msg>) (spec : WrapperSpec<'runner, 'model, 'msg, 'subModel, 'subMsg>)
                                : Wrapper<'msg, 'subMsg> =
    fun subMsg ->
        wrap <| makeOperate wrap spec subMsg
