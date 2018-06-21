[<AutoOpen>]
module Dap.Platform.Wrapping

open Elmish

// This warpper logic is mostly following elm-component-updater
// https://github.com/mpdairy/elm-component-updater/blob/master/src/Updater.elm

[<StructuredFormatDisplay("{SubMsg}")>]
type Wrapping<'runner, 'model, 'msg, 'subModel, 'subMsg> (wrap', spec', subMsg') =
    let wrap : Wrap<'runner, 'model, 'msg> = wrap'
    let spec : WrapperSpec<'runner, 'model, 'msg, 'subModel, 'subMsg> = spec'
    let subMsg : 'subMsg = subMsg'
    member _this.SubMsg = subMsg
    member _this.Operate =
        fun runner (model, cmd) ->
            let (subModel, subCmd) = spec.UpdateSub runner (spec.GetSub model) subMsg
            let (reactModel, reactCmd) = spec.ReactSub runner subMsg subModel (spec.SetSub subModel model)
            let mapSubCmd = fun (m : 'subMsg) ->
                wrap <| new Wrapping<'runner, 'model, 'msg, 'subModel, 'subMsg> (wrap, spec, m)
            let subCmd = Cmd.map mapSubCmd subCmd
            let cmd = Cmd.batch [cmd; subCmd; reactCmd]
            (reactModel, cmd)
    interface IWrapping<'runner, 'model, 'msg> with
        member this.Operate = this.Operate

let wrap (wrap : Wrap<'runner, 'model, 'msg>) (spec : WrapperSpec<'runner, 'model, 'msg, 'subModel, 'subMsg>)
                                : Wrapper<'msg, 'subMsg> =
    fun subMsg ->
        wrap <| new Wrapping<'runner, 'model, 'msg, 'subModel, 'subMsg> (wrap, spec, subMsg)
