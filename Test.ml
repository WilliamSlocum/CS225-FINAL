open Util
open StringSetMap
open Final

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
