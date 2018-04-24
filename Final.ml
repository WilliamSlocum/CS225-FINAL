(* Name: <William H Slocum> *)
(* Course: UVM CS 225 Spring 2018 - Darais *)
(* Final Project *)

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
