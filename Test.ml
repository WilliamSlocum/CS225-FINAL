open Util
open StringSetMap
open Final

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

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
    | _ -> print_string "STUCK"
  end
| _ -> print_string "STUCK"

(* ====================================================================== *)

let _ = print_string " " ;;
print_endline " " ;; print_string "--" ;; print_endline " " ;;
