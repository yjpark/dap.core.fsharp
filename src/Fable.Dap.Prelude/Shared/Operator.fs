[<AutoOpen>]
module Dap.Prelude.Operator

/// To remove some parenthesis
/// http://kevincantu.org/code/operators.html
let inline (^<|) f a = f a