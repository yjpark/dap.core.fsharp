[<AutoOpen>]
module Dap.Remote.Fable

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop
module TE = Thoth.Json.Encode
module TD = Thoth.Json.Decode
#else
module TE = Thoth.Json.Net.Encode
module TD = Thoth.Json.Net.Decode
#endif
open Dap.Prelude
open Dap.Context

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

type E with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member fable<'t> (v : 't) = fableToJson (v :> obj)
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member encodeFable (indent : int) (v : 't) = TE.encode indent <| E.fable<'t> v

type D with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
    static member fable<'t> (json : obj) : Result<'t, TD.DecoderError> =
        let json = fableObjToJson json
#else
    static member fable<'t> (json : Json) : Result<'t, TD.DecoderError> =
#endif
        try
            Ok <| fableCastJson<'t> json
        with e ->
            Error <| TD.FailMessage e.Message
