[<AutoOpen>]
module Dap.Context.Const

//PK = PropertyKind

[<Literal>]
let PK_Map = "Map"

[<Literal>]
let PK_List = "List"

[<Literal>]
let PK_Combo = "Combo"

[<Literal>]
let PK_Bool = "Bool"

[<Literal>]
let PK_Int = "Int"

[<Literal>]
let PK_Long = "Long"

[<Literal>]
let PK_String = "String"

let getCustomKind = sprintf "Custom<%s>"

