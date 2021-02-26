[<AutoOpen>]
module Dap.Context.ChannelsExtension

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type IChannels with
    member this.Add<'evt> (encoder : JsonEncoder<'evt>, decoder : JsonDecoder<'evt>, key : Key
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'evt>
            #endif
                ) =
    #if FABLE_COMPILER
        (ChannelSpec<'evt>.Create key encoder decoder, resolver)
    #else
        ChannelSpec<'evt>.Create key encoder decoder
    #endif
        |> this.Add<'evt>

type IChannels with
    member this.AddJson key = this.Add<Json> (E.json, D.json, key)
    member this.AddUnit key = this.Add<unit> (E.unit, D.unit, key)
    member this.AddBool key = this.Add<bool> (E.bool, D.bool, key)
    member this.AddString key = this.Add<string> (E.string, D.string, key)
    member this.AddInt key = this.Add<int> (E.int, D.int, key)
    member this.AddLong key = this.Add<int64> (E.long, D.long, key)
    member this.AddFloat key = this.Add<float> (E.float, D.float, key)
    member this.AddDecimal key = this.Add<decimal> (E.decimal, D.decimal, key)
