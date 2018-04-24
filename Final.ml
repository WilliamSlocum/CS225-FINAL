(* Name: <William H Slocum> *)
(* Course: UVM CS 225 Spring 2018 - Darais *)
(* Final Project *)

open Util
open StringSetMap

(* Syntax For Types *)

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
  | Lambda of var * exp
  | Apply of exp * exp
[@@deriving show {with_path = false}]

(* Γ ∈ tenv ≔ var ⇀ type *)
type tenv = ty string_map
[@@deriving show {with_path = false}]

(* C ≔ type ⇀ type *)
type constr = ty string_map
[@@deriving show {with_path = false}]

let rec infer (g : tenv) (e : exp) (c : constr) : constr = match e with

  | Var(x) -> raise TODO

  | Lambda(x,e) -> raise TODO

  | Apply(e1,e2) -> raise TODO

  | Zero -> raise TODO

  | Succ(e) -> raise TODO

  | Pred(e) -> raise TODO

  | IsZero(e) -> raise TODO

  | True -> raise TODO

  | False -> raise TODO

  | If(e1,e2,e3) -> raise TODO
