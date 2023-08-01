open Ast.Parsed
open Ast.Typed
module SMap = Map.Make (String)

exception TypeError of string

let throw_typeerr s = TypeError s |> raise

let rec string_of_typ = function
  | TUnit -> "unit"
  | TInt | TVar _ -> "int"
  | TArrow (t1, t2) -> string_of_typ t1 ^ " -> " ^ string_of_typ t2

let rec typechecker ctx (e : Ast.Parsed.t) =
  match e with
  | Const _ as t -> (t, TInt, ctx)
  | Var s -> (
      try (e, SMap.find s ctx, ctx)
      with Not_found -> throw_typeerr "Unbound Variable")
  | Def (s, e1) ->
      let _, t, _ = typechecker ctx e1 in
      let ctx' = SMap.add s t ctx in
      (e, TUnit, ctx')
  | Defn (s, e1, e2) ->
      let t = List.fold_left (fun acc _ -> TArrow (TInt, acc)) TInt e1 in
      let ctx' = SMap.add s t ctx in
      let ctx'' = List.fold_left (fun acc a -> SMap.add a TInt acc) ctx' e1 in
      let _, _, _ = typechecker ctx'' e2 in
      (e, TUnit, ctx'')
  | Fn (s, e1) ->
      let t = List.fold_left (fun acc _ -> TArrow (TInt, acc)) TInt s in
      let ctx' = List.fold_left (fun acc a -> SMap.add a TInt acc) ctx s in
      let _, _, _ = typechecker ctx' e1 in
      (e, t, ctx)
  | App (e1, e2) ->
      let _, t1, _ = typechecker ctx e1 in
      let rec next_arg last_type = function
        | [] -> last_type
        | a :: res -> (
            let _, t2, _ = typechecker ctx a in
            match last_type with
            | TArrow (t11, t12) ->
                if t11 = t2 then next_arg t12 res
                else throw_typeerr "Types dont match"
            | _ -> throw_typeerr "Not a function, cannot be applied")
      in
      (e, next_arg t1 e2, ctx)
