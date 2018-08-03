[<AutoOpen>]
module Dap.Remote.Util

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
#else
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
#endif

open Dap.Prelude
open Dap.Remote
open Dap.Remote.Internal

// http://fable.io/docs/interacting.html#json-serialization
#if FABLE_COMPILER
[<PassGenericsAttribute>]
let fableDecodeJson<'t> (str : string) =
    ofJson<'t> str

let fableEncodeJson v : string =
    toJson v

#else
open Newtonsoft.Json

let private fableJsonConverter = Fable.JsonConverter () :> JsonConverter

let fableDecodeJson<'t> (str : string) =
    JsonConvert.DeserializeObject<'t>(str, [| fableJsonConverter |])

let fableEncodeJson v : string =
    JsonConvert.SerializeObject(v, [| fableJsonConverter |])

#endif

let fableToJson v : Json =
    fableEncodeJson v |> parseJson

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let fableCastJson<'t> (v : Json)  =
    v.EncodeJson 0
    |> fableDecodeJson<'t>