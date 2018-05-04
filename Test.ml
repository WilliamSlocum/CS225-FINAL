(* Name: <William H Slocum> *)
(* Course: UVM CS 225 Spring 2018 - Darais *)
(* Final Project *)

open Util
open StringSetMap
open Final

(* Testing Suite for Final.ml *)

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = IsZero(Apply(LambdaA("x",Nat,Var("x")),Zero)) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = If(IsZero(Zero),Pred(Succ(Zero)),Succ(Zero)) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = IsZero(Apply(Lambda("X0", Var("X0")),Succ(Zero))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = Apply(Lambda("x",Var("x")),Lambda("y", Var("y"))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = If(IsZero(Zero),Pred(Succ(Zero)),Succ(Zero)) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = Lambda("x",Succ(Var("x"))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = Lambda("x",Succ(Var("x"))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = If(Pred(Zero),True,False)
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = Lambda("x",If(Var("x"),False,False)) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = Lambda("x",If(Var("x"),False,IsZero(False))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = Apply(Apply(Lambda("w",Var("w")),Lambda("x",Var("x"))),Apply(Lambda("y",Var("y")),Lambda("z",Var("z")))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = LambdaA("x",Bool,If(Var("x"),Var("x"),Var("x"))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;

print_endline " " ;; print_string "EXPRESSION" ;; print_endline " " ;;
let e = Lambda("x",IsZero(If(Var("x"),Zero,Pred(Zero)))) ;;
let _ = print_endline ([%show : exp] e) ;;

print_endline " " ;; print_string "TYPE * CONSTRAINT SET" ;; print_endline " " ;;
let v = infer (StringMap.empty) e (TermPairSet.empty) ;;
print_endline ([%show : result] v);;

print_endline " " ;; print_string "SOLUTION" ;; print_endline " " ;;
match v with
| Val(t,c) ->
  let u = unify c (TermPairSet.empty) in
  begin match u with
    | Val(c1,c2) ->
      print_endline ([%show : constr] c2)
    | _ -> print_endline "STUCK"
  end
| _ -> print_endline "STUCK"

(* ====================================================================== *)

let _ = print_endline " " ;;

(* Name: <William H Slocum> *)
