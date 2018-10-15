[<AutoOpen>]
module Demo.Types

open Dap.Prelude
open Dap.Context

(*
 * Generated: <ValueInterface>
 *)
type IPublisher =
    abstract Name : string with get
    abstract Year : int with get

(*
 * Generated: <ComboInterface>
 *)
type IPerson =
    abstract Name : IVarProperty<string> with get
    abstract Age : IVarProperty<int> with get

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type Publisher = {
    Name : (* IPublisher *) string
    Year : (* IPublisher *) int
} with
    static member Create name year
            : Publisher =
        {
            Name = (* IPublisher *) name
            Year = (* IPublisher *) year
        }
    static member Default () =
        Publisher.Create
            "" (* IPublisher *) (* name *)
            0 (* IPublisher *) (* year *)
    static member SetName ((* IPublisher *) name : string) (this : Publisher) =
        {this with Name = name}
    static member SetYear ((* IPublisher *) year : int) (this : Publisher) =
        {this with Year = year}
    static member UpdateName ((* IPublisher *) update : string -> string) (this : Publisher) =
        this |> Publisher.SetName (update this.Name)
    static member UpdateYear ((* IPublisher *) update : int -> int) (this : Publisher) =
        this |> Publisher.SetYear (update this.Year)
    static member JsonEncoder : JsonEncoder<Publisher> =
        fun (this : Publisher) ->
            E.object [
                "name", E.string (* IPublisher *) this.Name
                "year", E.int (* IPublisher *) this.Year
            ]
    static member JsonDecoder : JsonDecoder<Publisher> =
        D.object (fun get ->
            {
                Name = get.Optional.Field (* IPublisher *) "name" D.string
                    |> Option.defaultValue ""
                Year = get.Optional.Field (* IPublisher *) "year" D.int
                    |> Option.defaultValue 0
            }
        )
    static member JsonSpec =
        FieldSpec.Create<Publisher> (Publisher.JsonEncoder, Publisher.JsonDecoder)
    interface IJson with
        member this.ToJson () = Publisher.JsonEncoder this
    interface IObj
    member this.WithName ((* IPublisher *) name : string) =
        this |> Publisher.SetName name
    member this.WithYear ((* IPublisher *) year : int) =
        this |> Publisher.SetYear year
    interface IPublisher with
        member this.Name (* IPublisher *) : string = this.Name
        member this.Year (* IPublisher *) : int = this.Year
    member this.AsPublisher = this :> IPublisher

(*
 * Generated: <Class>
 *     IsFinal
 *)
type Author (owner : IOwner, key : Key) =
    inherit WrapProperties<Author, IComboProperty> ()
    let target = Properties.combo owner key
    let name = target.AddVar<(* IPerson *) string> (E.string, D.string, "name", "", None)
    let age = target.AddVar<(* IPerson *) int> (E.int, D.int, "age", 0, None)
    let publisher = target.AddVar<(* Author *) string> (E.string, D.string, "publisher", "", None)
    let books = target.AddCombo (* Author *) ("books")
    do (
        target.SealCombo ()
        base.Setup (target)
    )
    static member Create o k = new Author (o, k)
    static member Default () = Author.Create noOwner NoKey
    static member AddToCombo key (combo : IComboProperty) =
        combo.AddCustom<Author> (Author.Create, key)
    override this.Self = this
    override __.Spawn o k = Author.Create o k
    override __.SyncTo t = target.SyncTo t.Target
    member __.Name (* IPerson *) : IVarProperty<string> = name
    member __.Age (* IPerson *) : IVarProperty<int> = age
    member __.Publisher (* Author *) : IVarProperty<string> = publisher
    member __.Books (* Author *) : IComboProperty = books
    interface IPerson with
        member this.Name (* IPerson *) : IVarProperty<string> = this.Name
        member this.Age (* IPerson *) : IVarProperty<int> = this.Age
    member this.AsPerson = this :> IPerson

(*
 * Generated: <Union>
 *     IsJson
 *)
type Status =
    | Unknown
    | Written of author : string
    | Published of publisher : string * year : int * copies : int option
with
    static member CreateUnknown () : Status =
        Unknown
    static member CreateWritten author : Status =
        Written (author)
    static member CreatePublished publisher year copies : Status =
        Published (publisher, year, copies)
    static member JsonSpec' : CaseSpec<Status> list =
        [
            CaseSpec<Status>.Create ("Unknown", [])
            CaseSpec<Status>.Create ("Written", [
                S.string
            ])
            CaseSpec<Status>.Create ("Published", [
                S.string ; S.int ; (S.option (E.int, D.int))
            ])
        ]
    static member JsonEncoder = E.union Status.JsonSpec'
    static member JsonDecoder = D.union Status.JsonSpec'
    static member JsonSpec =
        FieldSpec.Create<Status> (Status.JsonEncoder, Status.JsonDecoder)
    interface IJson with
        member this.ToJson () = Status.JsonEncoder this