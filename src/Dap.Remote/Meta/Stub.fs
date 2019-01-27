[<RequireQualifiedAccess>]
module Dap.Remote.Meta.Stub

open Dap.Context.Meta

type StubBuilder () =
    inherit MetaBuilder<StubMeta> ()
    override __.Zero () = StubMeta.Create [] []
    [<CustomOperation("req")>]
    member __.Req (meta : StubMeta, kind : StubFieldKind, type' : string) =
        {meta with Req = meta.Req @ [ StubFieldMeta.Create kind type' ]}
    [<CustomOperation("evt")>]
    member __.Evt (meta : StubMeta, kind : StubFieldKind, type' : string) =
        {meta with Evt = meta.Evt @ [ StubFieldMeta.Create kind type' ]}
