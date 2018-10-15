[<AutoOpen>]
module Dap.Context.HandlersExtension

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type IHandlers with
    member this.Add<'req, 'res> (qe : JsonEncoder<'req>, qd : JsonDecoder<'req>, se : JsonEncoder<'res>, sd : JsonDecoder<'res>, key : Key) =
        HandlerSpec<'req, 'res>.Create key qe qd se sd
        |> this.Add<'req, 'res>

type IHandlers with
    member this.AddJsonJson key = this.Add<Json, Json> (E.value, D.value, E.value, D.value, key)
    member this.AddJsonUnit key = this.Add<Json, unit> (E.value, D.value, E.unit, D.unit, key)
    member this.AddJsonBool key = this.Add<Json, bool> (E.value, D.value, E.bool, D.bool, key)
    member this.AddJsonString key = this.Add<Json, string> (E.value, D.value, E.string, D.string, key)
    member this.AddJsonInt key = this.Add<Json, int> (E.value, D.value, E.int, D.int, key)
    member this.AddJsonLong key = this.Add<Json, int64> (E.value, D.value, E.long, D.long, key)
    member this.AddJsonFloat key = this.Add<Json, float> (E.value, D.value, E.float, D.float, key)
    member this.AddJsonDecimal key = this.Add<Json, decimal> (E.value, D.value, E.decimal, D.decimal, key)

type IHandlers with
    member this.AddUnitJson key = this.Add<unit, Json> (E.unit, D.unit, E.value, D.value, key)
    member this.AddUnitUnit key = this.Add<unit, unit> (E.unit, D.unit, E.unit, D.unit, key)
    member this.AddUnitBool key = this.Add<unit, bool> (E.unit, D.unit, E.bool, D.bool, key)
    member this.AddUnitString key = this.Add<unit, string> (E.unit, D.unit, E.string, D.string, key)
    member this.AddUnitInt key = this.Add<unit, int> (E.unit, D.unit, E.int, D.int, key)
    member this.AddUnitLong key = this.Add<unit, int64> (E.unit, D.unit, E.long, D.long, key)
    member this.AddUnitFloat key = this.Add<unit, float> (E.unit, D.unit, E.float, D.float, key)
    member this.AddUnitDecimal key = this.Add<unit, decimal> (E.unit, D.unit, E.decimal, D.decimal, key)

type IHandlers with
    member this.AddBoolJson key = this.Add<bool, Json> (E.bool, D.bool, E.value, D.value, key)
    member this.AddBoolUnit key = this.Add<bool, unit> (E.bool, D.bool, E.unit, D.unit, key)
    member this.AddBoolBool key = this.Add<bool, bool> (E.bool, D.bool, E.bool, D.bool, key)
    member this.AddBoolString key = this.Add<bool, string> (E.bool, D.bool, E.string, D.string, key)
    member this.AddBoolInt key = this.Add<bool, int> (E.bool, D.bool, E.int, D.int, key)
    member this.AddBoolLong key = this.Add<bool, int64> (E.bool, D.bool, E.long, D.long, key)
    member this.AddBoolFloat key = this.Add<bool, float> (E.bool, D.bool, E.float, D.float, key)
    member this.AddBoolDecimal key = this.Add<bool, decimal> (E.bool, D.bool, E.decimal, D.decimal, key)

type IHandlers with
    member this.AddStringJson key = this.Add<string, Json> (E.string, D.string, E.value, D.value, key)
    member this.AddStringUnit key = this.Add<string, unit> (E.string, D.string, E.unit, D.unit, key)
    member this.AddStringBool key = this.Add<string, bool> (E.string, D.string, E.bool, D.bool, key)
    member this.AddStringString key = this.Add<string, string> (E.string, D.string, E.string, D.string, key)
    member this.AddStringInt key = this.Add<string, int> (E.string, D.string, E.int, D.int, key)
    member this.AddStringLong key = this.Add<string, int64> (E.string, D.string, E.long, D.long, key)
    member this.AddStringFloat key = this.Add<string, float> (E.string, D.string, E.float, D.float, key)
    member this.AddStringDecimal key = this.Add<string, decimal> (E.string, D.string, E.decimal, D.decimal, key)

type IHandlers with
    member this.AddIntJson key = this.Add<int, Json> (E.int, D.int, E.value, D.value, key)
    member this.AddIntUnit key = this.Add<int, unit> (E.int, D.int, E.unit, D.unit, key)
    member this.AddIntBool key = this.Add<int, bool> (E.int, D.int, E.bool, D.bool, key)
    member this.AddIntString key = this.Add<int, string> (E.int, D.int, E.string, D.string, key)
    member this.AddIntInt key = this.Add<int, int> (E.int, D.int, E.int, D.int, key)
    member this.AddIntLong key = this.Add<int, int64> (E.int, D.int, E.long, D.long, key)
    member this.AddIntFloat key = this.Add<int, float> (E.int, D.int, E.float, D.float, key)
    member this.AddIntDecimal key = this.Add<int, decimal> (E.int, D.int, E.decimal, D.decimal, key)

type IHandlers with
    member this.AddLongJson key = this.Add<int64, Json> (E.long, D.long, E.value, D.value, key)
    member this.AddLongUnit key = this.Add<int64, unit> (E.long, D.long, E.unit, D.unit, key)
    member this.AddLongBool key = this.Add<int64, bool> (E.long, D.long, E.bool, D.bool, key)
    member this.AddLongString key = this.Add<int64, string> (E.long, D.long, E.string, D.string, key)
    member this.AddLongInt key = this.Add<int64, int> (E.long, D.long, E.int, D.int, key)
    member this.AddLongLong key = this.Add<int64, int64> (E.long, D.long, E.long, D.long, key)
    member this.AddLongFloat key = this.Add<int64, float> (E.long, D.long, E.float, D.float, key)
    member this.AddLongDecimal key = this.Add<int64, decimal> (E.long, D.long, E.decimal, D.decimal, key)

type IHandlers with
    member this.AddFloatJson key = this.Add<float, Json> (E.float, D.float, E.value, D.value, key)
    member this.AddFloatUnit key = this.Add<float, unit> (E.float, D.float, E.unit, D.unit, key)
    member this.AddFloatBool key = this.Add<float, bool> (E.float, D.float, E.bool, D.bool, key)
    member this.AddFloatString key = this.Add<float, string> (E.float, D.float, E.string, D.string, key)
    member this.AddFloatInt key = this.Add<float, int> (E.float, D.float, E.int, D.int, key)
    member this.AddFloatLong key = this.Add<float, int64> (E.float, D.float, E.long, D.long, key)
    member this.AddFloatFloat key = this.Add<float, float> (E.float, D.float, E.float, D.float, key)
    member this.AddFloatDecimal key = this.Add<float, decimal> (E.float, D.float, E.decimal, D.decimal, key)

type IHandlers with
    member this.AddDecimalJson key = this.Add<decimal, Json> (E.decimal, D.decimal, E.value, D.value, key)
    member this.AddDecimalUnit key = this.Add<decimal, unit> (E.decimal, D.decimal, E.unit, D.unit, key)
    member this.AddDecimalBool key = this.Add<decimal, bool> (E.decimal, D.decimal, E.bool, D.bool, key)
    member this.AddDecimalString key = this.Add<decimal, string> (E.decimal, D.decimal, E.string, D.string, key)
    member this.AddDecimalInt key = this.Add<decimal, int> (E.decimal, D.decimal, E.int, D.int, key)
    member this.AddDecimalLong key = this.Add<decimal, int64> (E.decimal, D.decimal, E.long, D.long, key)
    member this.AddDecimalFloat key = this.Add<decimal, float> (E.decimal, D.decimal, E.float, D.float, key)
    member this.AddDecimalDecimal key = this.Add<decimal, decimal> (E.decimal, D.decimal, E.decimal, D.decimal, key)