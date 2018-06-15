[<AutoOpen>]
module Dap.Remote.Packet

//NOTE: this file should be compiled in Dap.Fable.Remote and Dap.Platform.Remote

#if FABLE_COMPILER
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
#else
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
#endif

type Packet' with
    static member Create id kind payload = {
        Id = id 
        Kind = kind
        Payload = payload
    }
    static member Decoder =
        D.decode Packet'.Create
        |> D.required "i" D.string
        |> D.required "k" D.string
        |> D.required "p" D.string
    static member Encoder (this : Packet') =
        E.object [
            "i", E.string this.Id
            "k", E.string this.Kind
            "p", E.string this.Payload
        ]
    static member FromJsonString json =
        D.decodeString Packet'.Decoder json
    //Note: Can't declare as JsonRecord, since Packet'
    //is declared in different file (used by Client and Service)
    //interface JsonRecord with
    member this.ToJsonObject () =
        Packet'.Encoder this
    member this.ToJsonString (indent : int) =
        E.encode indent <| this.ToJsonObject ()

#if FABLE_COMPILER
let encode (pkt : Packet') : obj =
    box <| pkt.ToJsonString 0

let decode (json : obj) : Packet' =
    match Packet'.FromJsonString <| string json with
    | Ok pkt ->
        pkt
    | Error err ->
        failwith err
#else
let encode (pkt : Packet') : string =
    pkt.ToJsonString 0

let decode (json : string) : Packet' =
    match Packet'.FromJsonString <| json with
    | Ok pkt ->
        pkt
    | Error err ->
        failwith err
#endif