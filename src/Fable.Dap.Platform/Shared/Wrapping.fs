[<AutoOpen>]
module Dap.Platform.Wrapping

open Dap.Prelude

// This warpper logic is mostly following elm-component-updater
// https://github.com/mpdairy/elm-component-updater/blob/master/src/Updater.elm

[<StructuredFormatDisplay("{SubMsg}")>]
type Wrapping<'runner, 'model, 'msg, 'subModel, 'subMsg> (wrapMsg', spec', subMsg') =
    let wrapMsg : WrapMsg<'runner, 'model, 'msg> = wrapMsg'
    let spec : WrapperSpec<'runner, 'model, 'msg, 'subModel, 'subMsg> = spec'
    let subMsg : 'subMsg = subMsg'
    member __.SubMsg = subMsg
    member __.Operate =
        fun runner (model, cmd) ->
            let (subModel, subCmd) = spec.UpdateSub runner subMsg (spec.GetSub model)
            let (reactModel, reactCmd) = spec.ReactSub runner subMsg subModel (spec.SetSub subModel model)
            let mapSubCmd = fun (m : 'subMsg) ->
                wrapMsg <| new Wrapping<'runner, 'model, 'msg, 'subModel, 'subMsg> (wrapMsg, spec, m)
            let subCmd = mapCmd mapSubCmd subCmd
            let cmd = batchCmd [cmd; subCmd; reactCmd]
            (reactModel, cmd)
    interface IWrapping<'runner, 'model, 'msg> with
        member this.Operate = this.Operate

let wrap (wrapMsg : WrapMsg<'runner, 'model, 'msg>) (spec : WrapperSpec<'runner, 'model, 'msg, 'subModel, 'subMsg>)
                                : Wrapper<'msg, 'subMsg> =
    fun subMsg ->
        wrapMsg <| new Wrapping<'runner, 'model, 'msg, 'subModel, 'subMsg> (wrapMsg, spec, subMsg)
