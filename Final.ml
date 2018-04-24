(* Name: <William H Slocum> *)
(* Course: UVM CS 225 Spring 2018 - Darais *)
(* Final Project *)

open Util
open StringSetMap

(* Syntax For Types *)

(* X ∈ tvar ≈ 𝕊 *)
type tvar = string
[@@deriving show {with_path = false}]

type ty =
  | Bool
  | Nat
  | Prod of ty * ty
  | Fun of ty * ty
  | TVar of tvar
[@@deriving show {with_path = false}]

(* Syntax for expressions *)

(* x ∈ var ≈ 𝕊 *)
type var = string
[@@deriving show {with_path = false}]

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Zero
  | Succ of exp
  | Pred of exp
  | IsZero of exp
  | Var of var
  | Lambda of var * ty * exp
  | Apply of exp * exp
[@@deriving show {with_path = false}]

(* Γ ∈ tenv ≔ var ⇀ type *)
type tenv = ty string_map
[@@deriving show {with_path = false}]

(* C ≔ type ⇀ type *)
type constr = ty string_map
[@@deriving show {with_path = false}]

type result =
  | Val of ty * constr
  | Stuck
[@@deriving show {with_path = false}]

let rec infer (g : tenv) (e : exp) (c : constr) : result = match e with

  | Var(x) ->
    let t = StringMap.find x g in
    Val(t, c)

  | Lambda(x,t1,e') ->
    let g' = StringMap.add x t1 g in
    let v = infer g' e' c in begin match v with
      | Val(t2, c) -> Val(Fun(t1,t2),c)
      | _ -> Stuck
    end

  | Apply(e1,e2) -> raise TODO

  | Zero -> Val(Nat, c)

  | Succ(e') -> raise TODO
    (*
    let r = infer g e' c in begin match r with
      | Val(t, c) ->
        let c' = c union (t, Nat) in
        Val(Nat, c')
    | _ -> Stuck
    *)

  | Pred(e') -> raise TODO
  (*
  let r = infer g e' c in begin match r with
    | Val(t, c) ->
      let c' = c union (t, Nat) in
      Val(Nat, c')
    | _ -> Stuck
  *)

  | IsZero(e') -> raise TODO
  (*
  let r = infer g e' c in begin match r with
    | Val(t, c) ->
      let c' = c union (t, Nat) in
      Val(Nat, c')
    | _ -> Stuck
  *)

  | True -> Val(Bool, c)

  | False -> Val(Bool, c)

  | If(e1,e2,e3) -> raise TODO
