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

(* Create a Function that Creates Unique Type Variables *)
let n = ref 0 ;;

let uniqueVar() : string =
  let _ = n := !n + 1 in
  "X" ^ string_of_int !n ;;

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

  | Lambda(x,e') ->
    let xt = TVar(uniqueVar()) in
    let g' = StringMap.add x xt g in
    let v = infer g' e' c in begin match v with
      | Val(t, c') -> Val(Fun(xt,t),c')
      | _ -> Stuck
    end

  | Apply(e1,e2) ->
    let xt = TVar(uniqueVar()) in
    let v1 = infer g e1 c in
    let v2 = infer g e2 c in
      begin match v1 with
        | Val(t1,c1) -> begin match v2 with
          | Val(t2,c2) ->
            let c' = TermPairSet.add (t1, Fun(t2,xt)) (TermPairSet.union c1 c2) in
            Val(xt,c')
          | _ -> Stuck
          end
        | _ -> Stuck
      end

  | Zero -> Val(Nat, c)

  | Succ(e') ->
    let r = infer g e' c in begin match r with
      | Val(t, c1) ->
        let c' = TermPairSet.add (t, Nat) c1 in
        Val(Nat, c')
      | _ -> Stuck
    end

  | Pred(e') ->
    let r = infer g e' c in begin match r with
      | Val(t, c1) ->
        let c' = TermPairSet.add (t, Nat) c1 in
        Val(Nat, c')
      | _ -> Stuck
    end

  | IsZero(e') ->
    let r = infer g e' c in begin match r with
      | Val(t, c1) ->
        let c' = TermPairSet.add (t, Nat) c1 in
        Val(Nat, c')
      | _ -> Stuck
    end

  | True -> Val(Bool, c)

  | False -> Val(Bool, c)

  | If(e1,e2,e3) ->
  let v1 = infer g e1 c in
  let v2 = infer g e2 c in
  let v3 = infer g e3 c in
    begin match v1 with
      | Val(t1,c1) -> begin match v2 with
        | Val(t2,c2) -> begin match v3 with
          | Val(t3,c3) ->
            let c' = TermPairSet.add (t2, t3) (TermPairSet.add (t1, Bool) (TermPairSet.union (TermPairSet.union c1 c2) c3))  in
            Val(t2,c')
          | _ -> Stuck
          end
        | _ -> Stuck
        end
      | _ -> Stuck
    end

(*
let z = infer (StringMap.empty) (If(True,False,False)) (TermPairSet.empty) ;;
match z with | Val(a,b) -> print_endline ([%show : constr] b)
*)

(*
let x = Lambda("x",True) ;;
let z = infer (StringMap.empty) (If(True,x,x)) (TermPairSet.empty) ;;
let _ = print_endline ([%show : result] z)
*)

(*
let unify (c : int) : bool =
  if 1 = 1 then true else false ;;

let _ = print_endline ([%show : bool] (unify 1))
*)

(*
let rec unify (c : constr) : constr =
  if TermPairSet.is_empty c
  then c
  else
    let el = TermPairSet.choose c in
    let c' = TermPairSet.remove el c in
    let s = fst el in
    let t = snd el in
    if s = t then (unify c')
    else
      match s with
      | Fun(S1,S2) ->
        begin match t with
          | Fun(T1,T2) -> 0
          | _ -> FAIL
        end

      | TVar(X) ->
        if 0
        then 0
        else begin match t with
          | TVar(Y) ->
            if 0
            then 0
            else FAIL
          | _ -> FAIL
        end
  *)
