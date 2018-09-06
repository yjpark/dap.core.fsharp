[<RequireQualifiedAccess>]
module Dap.Context.Builder.List

open System.Reflection

open Dap.Prelude
open Dap.Context

type Builder<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) =
    inherit ObjBuilder<IListProperty<'p>> ()
    override __.Zero () =
        IListProperty<'p>.Empty spawner

    [<CustomOperation("add")>]
    member __.Properties (list: IListProperty<'p>, prop : 'p) =
        prop.SyncTo0 <| list.Add ()
        list
