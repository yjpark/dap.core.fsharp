[<AutoOpen>]
module Dap.Context.Const

//PK = PropertyKind

[<Literal>]
let PK_Combo = "Combo"

[<Literal>]
let PK_Bool = "Bool"

[<Literal>]
let PK_Int = "Int"

[<Literal>]
let PK_String = "String"

[<Literal>]
let PK_Float = "Float"

[<Literal>]
let PK_Decimal = "Decimal"

[<Literal>]
let PK_Long = "Long"

let getMapKind = sprintf "Map<%s>"

let getListKind = sprintf "List<%s>"

let getCustomKind = sprintf "Custom<%s>"
