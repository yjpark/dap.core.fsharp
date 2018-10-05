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

let fableDecodeJson<'t> (str : string) =
    TD.Auto.unsafeFromString<'t> (str)

let fableEncodeJson (indent : int) (v : obj) : string =
    TE.Auto.toString (indent, v)

let fableToJson (v : obj) : Json =
    fableEncodeJson 0 v |> parseJson

let fableCastJson<'t> (v : Json)  =
    v.EncodeJson 0
    |> fableDecodeJson<'t>

type E with
    static member fable<'t> (v : 't) = fableToJson (v :> obj)
    static member encodeFable (indent : int) (v : 't) = fableEncodeJson indent v

type D with
    static member fable<'t> (path : string) (json : Json) : Result<'t, TD.DecoderError> =
        try
            Ok <| fableCastJson<'t> json
        with e ->
            Error (path, TD.FailMessage e.Message)
