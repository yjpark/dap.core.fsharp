[<AutoOpen>]
module Dap.Context.ContextSpec

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context

[<AbstractClass>]
type ContextSpec (kind') =
    let kind : Kind = kind'
    let luid = newLuid kind
    interface IContextSpec with
        member __.Kind = kind
        member __.Luid = luid

type ContextSpec<'p when 'p :> IProperties> (kind, propertiesSpawner') =
    inherit ContextSpec (kind)
    let propertiesSpawner : PropertySpawner<'p> = propertiesSpawner'
    interface IContextSpec<'p> with
        member __.SpawnProperties owner = propertiesSpawner owner NoKey

type MapContextSpec<'p when 'p :> IProperty> (kind, spawner) =
    inherit ContextSpec<IDictProperty<'p>> (kind, Properties.map spawner)

type ListContextSpec<'p when 'p :> IProperty> (kind, spawner) =
    inherit ContextSpec<IListProperty<'p>> (kind, Properties.list spawner)

type ComboContextSpec (kind) =
    inherit ContextSpec<IComboProperty> (kind, Properties.combo)