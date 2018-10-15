[<AutoOpen>]
module Dap.Context.BaseContext

open Dap.Prelude
open Dap.Context.Internal
open Dap.Context.ContextSpec

[<AbstractClass>]
type DictContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<IDictProperty<'p>> and 'p :> IProperty> (logging, spec) =
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

type DictContext<'p when 'p :> IProperty> (logging, kind, spawner) =
    inherit DictContext<DictContext<'p>, DictContextSpec<'p>, 'p> (logging, new DictContextSpec<'p> (kind, spawner))
    override this.Self = this
    override __.Spawn l = new DictContext<'p> (l, kind, spawner)

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