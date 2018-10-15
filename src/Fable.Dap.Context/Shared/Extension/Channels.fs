[<AutoOpen>]
module Dap.Context.ChannelsExtension

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type IChannels with
    member this.AddUnit = this.Add<unit> (fun _ -> E.nil) (D.succeed ())
    member this.AddBool = this.Add<bool> E.bool D.bool
    member this.AddString = this.Add<string> E.string D.string
    member this.AddInt = this.Add<int> E.int D.int
#if !FABLE_COMPILER
    member this.AddLong = this.Add<int64> E.long D.long
#endif
    member this.AddFloat = this.Add<float> E.float D.float
    member this.AddDecimal = this.Add<decimal> E.decimal D.decimal
