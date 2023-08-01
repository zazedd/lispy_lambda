type variable = string

module Parsed = struct
  type t =
    | Const of int
    | Var of variable
    | Def of variable * t
    | Defn of variable * variable list * t
    | Fn of variable list * t
    | App of t * t list
end

module Typed = struct
  type typ = TUnit | TInt | TVar of typ | TArrow of typ * typ
  type expr = Parsed.t
  type t = expr * typ
end
