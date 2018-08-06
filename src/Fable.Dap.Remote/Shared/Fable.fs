[<AutoOpen>]
module Dap.Remote.Fable

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop
#endif

open Dap.Prelude
open Dap.Remote

// http://fable.io/docs/interacting.html#json-serialization
#if FABLE_COMPILER
[<PassGenericsAttribute>]
let fableDecodeJson<'t> (str : string) =
    ofJson<'t> str

let fableEncodeJson (v : obj) : string =
    toJson v

#else
open Newtonsoft.Json

let private fableJsonConverter = Fable.JsonConverter () :> JsonConverter

let fableDecodeJson<'t> (str : string) =
    JsonConvert.DeserializeObject<'t>(str, [| fableJsonConverter |])

let fableEncodeJson (v : obj) : string =
    JsonConvert.SerializeObject(v, [| fableJsonConverter |])

#endif

let fableToJson (v : obj) : Json =
    fableEncodeJson v |> parseJson

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let fableCastJson<'t> (v : Json)  =
    v.EncodeJson 0
    |> fableDecodeJson<'t>