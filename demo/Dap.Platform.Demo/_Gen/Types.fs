[<AutoOpen>]
module Dap.Platform.Demo.Types

open Dap.Context

(*
 * Generated: [ValueInterface] <IPublisher>
    {
        "name": "John Doe",
        "year": 2000
    }
 *)
type IPublisher =
    abstract Name : string with get
    abstract Year : int with get

(*
 * Generated: [ComboInterface] <IPerson>
    {
        "age": 30,
        "name": "John Doe"
    }
 *)
type IPerson =
    abstract Age : IVarProperty<int> with get
    abstract Name : IVarProperty<string> with get

(*
 * Generated: [Record] <Publisher>
 *     IsJson, IsLoose, IPublisher
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
    member this.WithName (name : string) = {this with Name = name}
    member this.WithYear (year : int) = {this with Year = year}
    interface IPublisher with
        member this.Name = this.Name
        member this.Year = this.Year

(*
 * Generated: [Class] <Author>
 *     IsFinal, IPerson
    {
        "age": 30,
        "name": "John Doe",
        "publisher": "No Publisher"
    }
 *)
type Author (owner : IOwner, key : Key) =
    inherit WrapProperties<Author, IComboProperty> ("Author")
    let target = Properties.combo owner key
    let age = target.AddInt "age" 30 None
    let name = target.AddString "name" "John Doe" None
    let publisher = target.AddString "publisher" "No Publisher" None
    do (
        target.SealCombo ()
        base.Setup (target)
    )
    static member Create o k = new Author (o, k)
    static member Empty () = Author.Create noOwner NoKey
    override this.Self = this
    override __.Spawn o k = Author.Create o k
    override __.SyncTo t = target.SyncTo t.Target
    member __.Age : IVarProperty<int> = age
    member __.Name : IVarProperty<string> = name
    member __.Publisher : IVarProperty<string> = publisher
    interface IPerson with
        member this.Age = this.Age
        member this.Name = this.Name