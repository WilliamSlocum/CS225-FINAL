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
  | LambdaA of var * ty * exp
  | Lambda of var * exp
  | Apply of exp * exp
[@@deriving show {with_path = false}]

(* Γ ∈ tenv ≔ var ⇀ type *)
type tenv = ty string_map
[@@deriving show {with_path = false}]

(* C ≔ {term = term} *)

let pp_pair
 (pp_a : Format.formatter -> 'a -> unit)
 (pp_b : Format.formatter -> 'b -> unit)
 (fmt : Format.formatter)
 (ab : ('a * 'b))
 : unit =
   let (a,b) = ab in
   Format.fprintf fmt "@[<2>(" ;
   pp_a fmt a ;
   Format.fprintf fmt ",@ " ;
   pp_a fmt b ;
   Format.fprintf fmt "@,)@]"

module TermPairSet = struct
  include Set.Make(struct type t = ty * ty let compare = Pervasives.compare end)
  let pp (fmt : Format.formatter) (ss : t) : unit =
    pp_set (pp_pair pp_ty pp_ty) fmt (elements ss)
end

type term_pair_set = TermPairSet.t
[@@deriving show {with_path=false}]

type constr = term_pair_set
[@@deriving show {with_path = false}]

type result =
  | Val of ty * constr
  | Stuck
[@@deriving show {with_path = false}]

let rec infer (g : tenv) (e : exp) (c : constr) : result = match e with

  | Var(x) ->
    let t = StringMap.find x g in
    Val(t, c)

  | LambdaA(x,t1,e') ->
    let g' = StringMap.add x t1 g in
    let v = infer g' e' c in begin match v with
      | Val(t2, c) -> Val(Fun(t1,t2),c)
      | _ -> Stuck
    end

  | Lambda(x,e') -> raise TODO
  (*
    Instantiate Type Variable X
    let g' = StringMap.add x X g in
    let v = infer g' e' c in begin match v with
      | Val(t, c) -> Val(Fun(X,t),c)
      | _ -> Stuck
    end

  *)

  | Apply(e1,e2) -> raise TODO
  (*
    let v1 = infer g e1 c in
    let v2 = infer g e2 c in
      begin match v1 with
        | Val(t1,c1) -> begin match v2 with
          | Val(t2,c2) ->
            let c' = c1 u c2 u (t1, Fun(t2,X)) in
            Val(X,c')
          | _ -> Stuck
        | _ -> Stuck
  *)

  | Zero -> Val(Nat, c)

  | Succ(e') ->
    let r = infer g e' c in begin match r with
      | Val(t, c1) ->
        let c' = TermPairSet.add (t, Nat) c1 in
        Val(Nat, c'')
      | _ -> Stuck
    end

  | Pred(e') ->
    let r = infer g e' c in begin match r with
      | Val(t, c1) ->
        let c' = TermPairSet.add (t, Nat) c1 in
        Val(Nat, c'')
      | _ -> Stuck
    end

  | IsZero(e') ->
    let r = infer g e' c in begin match r with
      | Val(t, c1) ->
        let c' = TermPairSet.add (t, Nat) c1 in
        Val(Nat, c'')
      | _ -> Stuck
    end

  | True -> Val(Bool, c)

  | False -> Val(Bool, c)

  | If(e1,e2,e3) -> raise TODO
  (*
  let v1 = infer g e1 c in
  let v2 = infer g e2 c in
  let v3 = infer g e3 c in
    begin match v1 with
      | Val(t1,c1) -> begin match v2 with
        | Val(t2,c2) -> begin match v3 with
          | Val(t3,c3) ->
            let c' = c1 u c2 u c3 u (t1, Bool) u (t2, t3) in
            Val(t2,c')
          | _ -> Stuck
        | _ -> Stuck
      | _ -> Stuck
  *)
