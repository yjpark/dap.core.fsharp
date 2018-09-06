[<RequireQualifiedAccess>]
module Dap.Context.Builder.Union

open System.Reflection

open Dap.Prelude
open Dap.Context

type CaseProperty (owner : IOwner, key : Key) =
    inherit WrapProperties<CaseProperty, IComboProperty> ("CaseProperty")
    let target = Properties.combo owner key
    let kind = target.AddString ("kind", "N/A", None)
    let fields = target.AddCombo ("fields")
    do (
        target.SealCombo ()
        base.Setup (target)
    )
    static member Create o k = new CaseProperty (o, k)
    static member Empty () = CaseProperty.Create noOwner NoKey
    override this.Self = this
    override __.Spawn o k = CaseProperty.Create o k
    override __.SyncTo t = target.SyncTo t.Target
    member __.Kind : IVarProperty<string> = kind
    member __.Fields : IComboProperty = fields

type Builder () =
    inherit ObjBuilder<IListProperty<CaseProperty>> ()
    override __.Zero () =
        IListProperty<CaseProperty>.Empty CaseProperty.Create
    [<CustomOperation("case")>]
    member __.Case (list : IListProperty<CaseProperty>, kind, fields : IComboProperty) =
        let case = list.Add ()
        case.Kind.SetValue kind |> ignore
        case.Fields.SyncWith fields
        list
    [<CustomOperation("kind")>]
    member __.Kind (list : IListProperty<CaseProperty>, kind) =
        let case = list.Add ()
        case.Kind.SetValue kind |> ignore
        list
