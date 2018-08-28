[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Context

open Dap.Prelude
open Dap.Context.Internal
open Dap.Context.ContextSpec

[<AbstractClass>]
type MapContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<IMapProperty<'p>> and 'p :> IProperty> (logging, spec) =
    inherit Context<'c, 's, IMapProperty<'p>> (logging, spec)

[<AbstractClass>]
type ListContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<IListProperty<'p>> and 'p :> IProperty> (logging, spec) =
    inherit Context<'c, 's, IListProperty<'p>> (logging, spec)

[<AbstractClass>]
type ComboContext<'c, 's when 'c :> IContext and 's :> IContextSpec<IComboProperty>> (logging, spec) =
    inherit Context<'c, 's, IComboProperty> (logging, spec)

type MapContext<'p when 'p :> IProperty> (logging, kind, spawner) =
    inherit MapContext<MapContext<'p>, MapContextSpec<'p>, 'p> (logging, new MapContextSpec<'p> (kind, spawner))
    override this.Self = this
    override __.Spawn l = new MapContext<'p> (l, kind, spawner)

type ListContext<'p when 'p :> IProperty> (logging, kind, spawner) =
    inherit ListContext<ListContext<'p>, ListContextSpec<'p>, 'p> (logging, new ListContextSpec<'p> (kind, spawner))
    override this.Self = this
    override __.Spawn l = new ListContext<'p> (l, kind, spawner)

type ComboContext (logging, kind) =
    inherit ComboContext<ComboContext, ComboContextSpec> (logging, new ComboContextSpec (kind))
    override this.Self = this
    override __.Spawn l = new ComboContext (l, kind)

let map<'p when 'p :> IProperty> kind (spawner : PropertySpawner<'p>) =
    new MapContext<'p> (getLogging (), kind, spawner)

let list<'p when 'p :> IProperty> kind (spawner : PropertySpawner<'p>) =
    new ListContext<'p> (getLogging (), kind, spawner)

let combo kind =
    new ComboContext (getLogging (), kind)

type IContext with
    static member Empty kind = combo kind :> IContext