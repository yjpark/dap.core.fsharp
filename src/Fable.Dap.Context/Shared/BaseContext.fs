[<AutoOpen>]
module Dap.Context.BaseContext

open Dap.Prelude
open Dap.Context.Internal
open Dap.Context.ContextSpec

[<AbstractClass>]
type DictContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<IDictProperty<'p>> and 'p :> IProperty> (logging, spec) =
    inherit BaseContext<'c, 's, IDictProperty<'p>> (logging, spec)

[<AbstractClass>]
type DictContext<'c, 'p when 'c :> IContext and 'p :> IProperty> (logging, kind, spawner) =
    inherit DictContext<'c, DictContextSpec<'p>, 'p> (logging, new DictContextSpec<'p> (kind, spawner))

[<AbstractClass>]
type ListContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<IListProperty<'p>> and 'p :> IProperty> (logging, spec) =
    inherit BaseContext<'c, 's, IListProperty<'p>> (logging, spec)

[<AbstractClass>]
type ListContext<'c, 'p when 'c :> IContext and 'p :> IProperty> (logging, kind, spawner) =
    inherit ListContext<'c, ListContextSpec<'p>, 'p> (logging, new ListContextSpec<'p> (kind, spawner))

[<AbstractClass>]
type ComboContext<'c, 's when 'c :> IContext and 's :> IContextSpec<IComboProperty>> (logging, spec) =
    inherit BaseContext<'c, 's, IComboProperty> (logging, spec)

[<AbstractClass>]
type ComboContext<'c when 'c :> IContext> (logging, kind) =
    inherit ComboContext<'c, ComboContextSpec> (logging, new ComboContextSpec (kind))

[<AbstractClass>]
type CustomContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<'p> and 'p :> ICustomProperties> (logging, spec) =
    inherit BaseContext<'c, 's, 'p> (logging, spec)

[<AbstractClass>]
type CustomContext<'c, 'p when 'c :> IContext and 'p :> ICustomProperties> (logging, kind, spawner) =
    inherit CustomContext<'c, ContextSpec<'p>, 'p> (logging, new ContextSpec<'p> (kind, spawner))

[<AbstractClass>]
type EmptyContext<'c, 's when 'c :> IContext and 's :> IContextSpec<NoProperties>> (logging, spec) =
    inherit BaseContext<'c, 's, NoProperties> (logging, spec)

[<AbstractClass>]
type EmptyContext<'c when 'c :> IContext> (logging, kind) =
    inherit EmptyContext<'c, ContextSpec<NoProperties>> (logging, new ContextSpec<NoProperties> (kind, NoProperties.Create))

type DictContext<'p when 'p :> IProperty> (logging, kind, spawner) =
    inherit DictContext<DictContext<'p>, 'p> (logging, kind, spawner)
    override this.Self = this
    override __.Spawn l = new DictContext<'p> (l, kind, spawner)

type ListContext<'p when 'p :> IProperty> (logging, kind, spawner) =
    inherit ListContext<ListContext<'p>, 'p> (logging, kind, spawner)
    override this.Self = this
    override __.Spawn l = new ListContext<'p> (l, kind, spawner)

type ComboContext (logging, kind) =
    inherit ComboContext<ComboContext> (logging, kind)
    override this.Self = this
    override __.Spawn l = new ComboContext (l, kind)

type CustomContext<'p when 'p :> ICustomProperties> (logging, kind, spawner) =
    inherit CustomContext<CustomContext<'p>, 'p> (logging, kind, spawner)
    override this.Self = this
    override __.Spawn l = new CustomContext<'p> (l, kind, spawner)

type EmptyContext (logging, kind) =
    inherit EmptyContext<EmptyContext> (logging, kind)
    override this.Self = this
    override __.Spawn l = new EmptyContext (l, kind)

