[<AutoOpen>]
module Dap.Platform.Cmd

module Cmd = Elmish.Cmd

let noCmd = Cmd.none

let cmdOfMsg = Cmd.ofMsg

let cmdOfSub = Cmd.ofSub

let mapCmd = Cmd.map

let batchCmd = Cmd.batch