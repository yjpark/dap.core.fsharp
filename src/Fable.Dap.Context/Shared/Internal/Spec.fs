[<AutoOpen>]
module Dap.Context.Internal.Spec

open System

open Dap.Prelude
open Dap.Context

type internal AspectSpec internal (luid', key') =
    let luid : Luid = luid'
    let key : Key = key'
    static member Create0 key =
        new AspectSpec (key, key)
        :> IAspectSpec
    override this.ToString () =
        sprintf "[AspectSpec:K='%s',L='%s']" key luid
    interface IAspectSpec with
        member __.Luid = luid
        member __.Key = key

type IAspectSpec with
    static member CalcSubLuid (parentLuid : Luid) (subKey : Key) =
        sprintf "%s.%s" parentLuid subKey
    member this.GetSubSpec subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        new AspectSpec (luid, subKey)
        :> IAspectSpec
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        new AspectSpec (luid, this.Key)
        :> IAspectSpec

type internal PropertySpec internal (luid, key, initValue') =
    inherit AspectSpec (luid, key)
    let initValue : Json = initValue'
    static member Create1 key initValue =
        new PropertySpec (key, key, initValue)
        :> IPropertySpec
    override this.ToString () =
        sprintf "[PropertySpec:K='%s',L='%s',V='%s']" key luid <| initValue.ToString ()
    interface IPropertySpec with
        member __.InitValue = initValue

type IPropertySpec with
    member this.GetSubSpec subKey =
        let luid = IAspectSpec.CalcSubLuid this.Luid subKey
        new PropertySpec (luid, subKey, this.InitValue)
        :> IPropertySpec
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = IAspectSpec.CalcSubLuid parent.Luid this.Key
        new PropertySpec (luid, this.Key, this.InitValue)
        :> IPropertySpec
    member this.ForClone key =
        new PropertySpec (key, key, this.InitValue)
        :> IPropertySpec

type internal ChannelSpec<'evt> internal (luid, key, encoder', decoder') =
    inherit AspectSpec (luid, key)
    let encoder : JsonEncoder<'evt> = encoder'
    let decoder : JsonDecoder<'evt> = decoder'
    static member Create key encoder decoder =
        new ChannelSpec<'evt> (key, key, encoder, decoder)
        :> IChannelSpec<'evt>
    override this.ToString () =
#if FABLE_COMPILER
        sprintf "[ChannelSpec<_>:K='%s',L='%s']" key luid
#else
        sprintf "[ChannelSpec<%s>:K='%s',L='%s']" typeof<'evt>.FullName key luid
#endif
    interface IChannelSpec<'evt> with
        member __.Encoder = encoder
        member __.Decoder = decoder
#if !FABLE_COMPILER
        member __.EvtType = typeof<'evt>
#endif

type IChannelSpec<'evt> with
    member this.GetSubSpec subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        new ChannelSpec<'evt> (luid, subKey, this.Encoder, this.Decoder)
        :> IChannelSpec<'evt>
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        new ChannelSpec<'evt> (luid, this.Key, this.Encoder, this.Decoder)
        :> IChannelSpec<'evt>
    member this.ForClone key =
        new ChannelSpec<'evt> (key, key, this.Encoder, this.Decoder)
        :> IChannelSpec<'evt>

type internal HandlerSpec<'req, 'res> internal (luid, key, reqEncoder', reqDecoder', resEncoder', resDecoder') =
    inherit AspectSpec (luid, key)
    let reqEncoder : JsonEncoder<'req> = reqEncoder'
    let reqDecoder : JsonDecoder<'req> = reqDecoder'
    let resEncoder : JsonEncoder<'res> = resEncoder'
    let resDecoder : JsonDecoder<'res> = resDecoder'
    static member Create key qe qd se sd=
        new HandlerSpec<'req, 'res> (key, key, qe, qd, se, sd)
        :> IHandlerSpec<'req, 'res>
    override this.ToString () =
#if FABLE_COMPILER
        sprintf "[HandlerSpec<_,_>:K='%s',L='%s']" key luid
#else
        sprintf "[HandlerSpec<%s,%s>:K='%s',L='%s']" typeof<'req>.FullName typeof<'res>.FullName key luid
#endif
    interface IHandlerSpec<'req, 'res> with
        member __.ReqEncoder = reqEncoder
        member __.ReqDecoder = reqDecoder
        member __.ResEncoder = resEncoder
        member __.ResDecoder = resDecoder
#if !FABLE_COMPILER
        member __.ReqType = typeof<'req>
        member __.ResType = typeof<'res>
#endif

type IHandlerSpec<'req, 'res> with
    member this.GetSubSpec subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        new HandlerSpec<'req, 'res> (luid, subKey, this.ReqEncoder, this.ReqDecoder, this.ResEncoder, this.ResDecoder)
        :> IHandlerSpec<'req, 'res>
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        new HandlerSpec<'req, 'res> (luid, this.Key, this.ReqEncoder, this.ReqDecoder, this.ResEncoder, this.ResDecoder)
        :> IHandlerSpec<'req, 'res>
    member this.ForClone key =
        new HandlerSpec<'req, 'res> (key, key, this.ReqEncoder, this.ReqDecoder, this.ResEncoder, this.ResDecoder)
        :> IHandlerSpec<'req, 'res>

