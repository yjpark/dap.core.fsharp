[<AutoOpen>]
module Dap.Platform.Demo.Types

open Dap.Context

(*
 * Generated: Record<Publisher>
 *     IsJson, IsLoose
    {
        "name": "John Doe",
        "year": 2000
    }
 *)
type Publisher = {
    Name : string
    Year : int
} with
    static member Create name year
            : Publisher =
        {
            Name = name
            Year = year
        }
    static member Default () =
        Publisher.Create "John Doe" 2000
    static member JsonEncoder : JsonEncoder<Publisher> =
        fun (this : Publisher) ->
            E.object [
                "name", E.string this.Name
                "year", E.int this.Year
            ]
    static member JsonDecoder : JsonDecoder<Publisher> =
        D.decode Publisher.Create
        |> D.optional "name" D.string "John Doe"
        |> D.optional "year" D.int 2000
    interface IJson with
        member this.ToJson () = Publisher.JsonEncoder this
    static member FieldSpec =
        FieldSpec.Create<Publisher>
            Publisher.JsonEncoder Publisher.JsonDecoder
    member this.WithName name = {this with Name = name}
    member this.WithYear year = {this with Year = year}

(*
 * Generated: Class<PublisherProperty>
 *     IsFinal
    {
        "name": "John Doe",
        "year": 2000
    }
 *)
type PublisherProperty (owner : IOwner, key : Key) =
    inherit WrapProperties<PublisherProperty, IComboProperty> ("PublisherProperty")
    let target = Properties.combo owner key
    let name = target.AddString "name" "John Doe" None
    let year = target.AddInt "year" 2000 None
    do (
        target.SealCombo ()
        base.Setup (target)
    )
    static member Create o k = new PublisherProperty (o, k)
    static member Empty () = PublisherProperty.Create noOwner NoKey
    override this.Self = this
    override __.Spawn o k = PublisherProperty.Create o k
    override __.SyncTo t = target.SyncTo t.Target
    member __.Name = name
    member __.Year = year