[<AutoOpen>]
module Dap.Context.Context

open Dap.Prelude
open Dap.Context.Internal
open Dap.Context.ContextSpec

[<AbstractClass>]
type MapContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<IDictProperty<'p>> and 'p :> IProperty> (logging, spec) =
    inherit BaseContext<'c, 's, IDictProperty<'p>> (logging, spec)

[<AbstractClass>]
type ListContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<IListProperty<'p>> and 'p :> IProperty> (logging, spec) =
    inherit BaseContext<'c, 's, IListProperty<'p>> (logging, spec)

[<AbstractClass>]
type ComboContext<'c, 's when 'c :> IContext and 's :> IContextSpec<IComboProperty>> (logging, spec) =
    inherit BaseContext<'c, 's, IComboProperty> (logging, spec)

[<AbstractClass>]
type CustomContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<'p> and 'p :> ICustomProperties> (logging, spec) =
    inherit BaseContext<'c, 's, 'p> (logging, spec)

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

type CustomContext<'p when 'p :> ICustomProperties> (logging, kind, spawner) =
    inherit CustomContext<CustomContext<'p>, ContextSpec<'p>, 'p> (logging, new ContextSpec<'p> (kind, spawner))
    override this.Self = this
    override __.Spawn l = new CustomContext<'p> (l, kind, spawner)