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
        Val(Bool, c')
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
let unify (c : int) : bool =
  if 1 = 1 then true else false ;;

let _ = print_endline ([%show : bool] (unify 1))
*)

let rec tsubst (zt : tvar) (t : ty) (tS : ty) : ty = match t with
  | Bool -> Bool
  | Nat -> Nat
  | TVar(xt) ->
    if xt = zt
    then tS
    else t
  | Fun(s1,s2) ->
    let s1' = tsubst zt s1 tS in
    let s2' = tsubst zt s2 tS in
    Fun(s1',s2')

let rec csubst (zt : tvar) (t : ty) (c : constr) : constr =
  if TermPairSet.is_empty c
  then c
  else
    let el = TermPairSet.choose c in
    let c' = TermPairSet.remove el c in
    begin match el with
      | (t1,t2) ->
        begin match t1 with
          | Bool ->
          begin match t2 with
            | Bool -> TermPairSet.add (t1,t2) (csubst zt t c')
            | Nat -> TermPairSet.add (t1,t2) (csubst zt t c')
            | TVar(yt) ->
              if yt = zt
              then TermPairSet.add (t1,t) (csubst zt t c')
              else TermPairSet.add (t1,t2) (csubst zt t c')
            | Fun(s1,s2) ->
              TermPairSet.add (t1, Fun((tsubst zt s1 t ),(tsubst zt s2 t))) (csubst zt t c')
          end

          | Nat ->
          begin match t2 with
            | Bool -> TermPairSet.add (t1,t2) (csubst zt t c')
            | Nat -> TermPairSet.add (t1,t2) (csubst zt t c')
            | TVar(yt) ->
              if yt = zt
              then TermPairSet.add (t1,t) (csubst zt t c')
              else TermPairSet.add (t1,t2) (csubst zt t c')
            | Fun(s1,s2) ->
              TermPairSet.add (t1, Fun((tsubst zt s1 t ),(tsubst zt s2 t))) (csubst zt t c')
          end

          | TVar(xt) ->
            begin match t2 with
              | Bool ->
                if xt = zt
                then TermPairSet.add (t,t2) (csubst zt t c')
                else TermPairSet.add (t1,t2) (csubst zt t c')
              | Nat ->
                if xt = zt
                then TermPairSet.add (t,t2) (csubst zt t c')
                else TermPairSet.add (t1,t2) (csubst zt t c')
              | TVar(yt) ->
                if xt = zt
                then
                  if yt = zt
                  then TermPairSet.add (t,t) (csubst zt t c')
                  else TermPairSet.add (t,t2) (csubst zt t c')
                else
                  if yt = zt
                  then TermPairSet.add (t1,t) (csubst zt t c')
                  else TermPairSet.add (t1,t2) (csubst zt t c')
              | Fun(s1,s2) ->
                if xt = zt
                then TermPairSet.add (t, Fun((tsubst zt s1 t ),(tsubst zt s2 t))) (csubst zt t c')
                else TermPairSet.add (t1, Fun((tsubst zt s1 t ),(tsubst zt s2 t))) (csubst zt t c')
            end

          | Fun(r1,r2) ->
            begin match t2 with
              | Bool -> TermPairSet.add (Fun((tsubst zt r1 t ),(tsubst zt r2 t)),t2) (csubst zt t c')
              | Nat -> TermPairSet.add (Fun((tsubst zt r1 t ),(tsubst zt r2 t)),t2) (csubst zt t c')
              | TVar(yt) ->
                if yt = zt
                then TermPairSet.add (Fun((tsubst zt r1 t ),(tsubst zt r2 t)),t) (csubst zt t c')
                else TermPairSet.add (Fun((tsubst zt r1 t ),(tsubst zt r2 t)),t2) (csubst zt t c')
              | Fun(s1,s2) ->
                TermPairSet.add (Fun((tsubst zt r1 t ),(tsubst zt r2 t)),Fun((tsubst zt s1 t ),(tsubst zt s2 t))) (csubst zt t c')
            end
        end
    end

let rec occurCheck (xt : tvar) (t : ty) : bool = match t with
  | Bool -> true
  | Nat -> true
  | TVar(yt) ->
    if xt = yt
    then false
    else true
  | Fun(t1,t2) ->
    let b1 = occurCheck xt t1 in
    let b2 = occurCheck xt t2 in
    if b1
    then
      if b2
      then true
      else false
    else false

type uresult =
  | Val of constr * constr
  | Stuck
[@@deriving show {with_path = false}]

(* MUST STILL IMPLEMENT X !E FV(T) *)
let rec unify (c : constr) (sb : constr) : uresult =
  if TermPairSet.is_empty c
  then Val(c,sb)
  else
    let el = TermPairSet.choose c in
    let c' = TermPairSet.remove el c in
    let s = fst el in
    let t = snd el in
    begin match s with
    | Bool ->
      begin match t with
        | Bool -> unify c' sb
        | Nat -> Stuck
        | TVar(yt) -> unify (csubst yt s c') (TermPairSet.add (TVar(yt),Bool) (csubst yt s sb))
        | Fun(t1,t2) -> Stuck
      end
    | Nat ->
      begin match t with
        | Bool -> Stuck
        | Nat -> unify c' sb
        | TVar(yt) -> unify (csubst yt s c') (TermPairSet.add (TVar(yt),Nat) (csubst yt s sb))
        | Fun(t1,t2) -> Stuck
      end
    | TVar(xt) ->
      begin match t with
        | Bool -> unify (csubst xt t c') (TermPairSet.add (TVar(xt),Bool) (csubst xt t sb))
        | Nat -> unify (csubst xt t c') (TermPairSet.add (TVar(xt),Nat) (csubst xt t sb))
        | TVar(yt) ->
          if xt = yt
          then unify c' sb
          else unify (csubst xt t c') (TermPairSet.add (TVar(xt), t) (csubst xt t sb))
        | Fun(t1,t2) ->
          if (occurCheck xt t)
          then unify (csubst xt t c') (TermPairSet.add (TVar(xt), t) (csubst xt t sb))
          else Stuck
      end
    | Fun(s1,s2) ->
      begin match t with
        | Bool -> Stuck
        | Nat -> Stuck
        | TVar(yt) ->
          if (occurCheck yt s)
          then unify (csubst yt s c') (TermPairSet.add (TVar(yt),s) (csubst yt s sb))
          else Stuck
        | Fun(t1,t2) -> unify (TermPairSet.union c' (TermPairSet.add (s2,t2) (TermPairSet.singleton (s1,t1)))) sb
      end
    end

let _ = print_string "--" ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = IsZero(Apply(LambdaA("x",Nat,Var("x")),Zero)) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      let u = unify c (TermPairSet.empty) in print_endline ([%show : constr] c2)
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

let _ = print_endline ([%show : string] "---------------------------------") ;;

let e = If(IsZero(Zero),Pred(Succ(Zero)),Succ(Zero)) ;;
let c = infer (StringMap.empty) e (TermPairSet.empty) ;;
let _ = print_endline ([%show : result] c) ;;
match c with | Val(t,c) -> let u = unify c (TermPairSet.empty) in print_endline ([%show : uresult] u)

let _ = print_endline ([%show : string] "---------------------------------") ;;

let e = IsZero(Apply(Lambda("X0", Var("X0")),Succ(Zero))) ;;
let c = infer (StringMap.empty) e (TermPairSet.empty) ;;
let _ = print_endline ([%show : result] c) ;;
match c with | Val(t,c) -> let u = unify c (TermPairSet.empty) in print_endline ([%show : uresult] u)

let _ = print_endline ([%show : string] "---------------------------------") ;;

let e = Apply(Lambda("x",Var("x")),Lambda("y", Var("y"))) ;;
let c = infer (StringMap.empty) e (TermPairSet.empty) ;;
let _ = print_endline ([%show : result] c) ;;
match c with | Val(t,c) -> let u = unify c (TermPairSet.empty) in print_endline ([%show : uresult] u)

let _ = print_endline ([%show : string] "---------------------------------") ;;

let e = If(IsZero(Zero),Pred(Succ(Zero)),Succ(Zero)) ;;
let c = infer (StringMap.empty) e (TermPairSet.empty) ;;
let _ = print_endline ([%show : result] c) ;;
match c with | Val(t,c) -> let u = unify c (TermPairSet.empty) in print_endline ([%show : uresult] u)

let _ = print_endline ([%show : string] "---------------------------------") ;;

let e = Lambda("x",Succ(Var("x"))) ;;
let c = infer (StringMap.empty) e (TermPairSet.empty) ;;
let _ = print_endline ([%show : result] c) ;;
match c with | Val(t,c) -> let u = unify c (TermPairSet.empty) in print_endline ([%show : uresult] u)

let _ = print_endline ([%show : string] "---------------------------------") ;;

(*
let c : constr = TermPairSet.add (TVar("z"),(Fun(TVar("u"),TVar("w")))) (TermPairSet.singleton (Fun(TVar("x"),TVar("y")),Fun(TVar("y"),TVar("z")))) ;;
let w = unify c (TermPairSet.empty) ;;
let _ = print_endline ([%show : uresult] w) ;;

let c : constr = TermPairSet.singleton (TVar("x"),Bool)
let w = unify c (TermPairSet.empty) ;;
let _ = print_endline ([%show : uresult] w) ;;

let c : constr = TermPairSet.singleton (Fun(Nat,Nat),Fun(TVar("x"),TVar("y")))
let w = unify c (TermPairSet.empty) ;;
let _ = print_endline ([%show : uresult] w) ;;
*)

(* Name: <William H Slocum> *)
