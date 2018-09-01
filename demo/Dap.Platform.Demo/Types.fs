[<AutoOpen>]
module Dap.Platform.Demo.Types

open Dap.Context

(*
 * Generated: Record<PublisherRecord>
 *     IsJson
    {
        "name": "John Doe",
        "year": 2000
    }
 *)
type PublisherRecord = {
    Name : string
    Year : int
} with
    static member Create name year
            : PublisherRecord =
        {
            Name = name
            Year = year
        }
    static member Default () =
        PublisherRecord.Create "John Doe" 2000
    member this.WithName name = {this with Name = name}
    member this.WithYear year = {this with Year = year}

(*
 * Generated: Class<Publisher>
 *     IsFinal
    {
        "name": "John Doe",
        "year": 2000
    }
 *)
type Publisher (owner : IOwner, key : Key) =
    inherit WrapProperties<Publisher, IComboProperty> ("Publisher")
    let target = Properties.combo owner key
    let name = target.AddString "name" "John Doe" None
    let year = target.AddInt "year" 2000 None
    do (
        target.SealCombo ()
        base.Setup (target)
    )
    static member Create o k = new Publisher (o, k)
    static member Empty () = Publisher.Create noOwner NoKey
    override this.Self = this
    override __.Spawn o k = Publisher.Create o k
    override __.SyncTo t = target.SyncTo t.Target
    member __.Name = name
    member __.Year = year