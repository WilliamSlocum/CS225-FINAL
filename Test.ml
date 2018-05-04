(* Name: <William H Slocum> *)
(* Course: UVM CS 225 Spring 2018 - Darais *)
(* Final Project *)

open Util
open StringSetMap
open Final

(* Testing Suite for Final.ml *)

(* ====================================================================== *)

(* Function to Perform Substitution on a Type Given a Unification Solution *)
let rec solvetype (c : constr) (t : ty) : ty =
  if TermPairSet.is_empty c
  then t
  else
    let el = TermPairSet.choose c in
    let c' = TermPairSet.remove el c in
    begin match el with
      | (xT,tS) ->
        begin match xT with
          | TVar(xt) ->
            let t' = tsubst xt t tS in
            solvetype c' t'
          | _ -> solvetype c' t
        end
    end

let tests : (exp) list =
  [If(Zero,True,False);
   If(IsZero(Zero),Pred(Zero),Succ(Zero));
   If(True,Lambda("x",Var("x")),Lambda("y",Var("y")));
   Succ(Lambda("x",Succ(Var("x"))));
   Pred(Lambda("x",Succ(Var("x"))));
   IsZero(Apply(Lambda("x", Var("x")),Succ(Zero)));
   Lambda("x",Succ(Var("x")));
   Lambda("x",If(Var("x"),True,Var("x")));
   Lambda("x",IsZero(If(Var("x"),Zero,Pred(Zero))));
   Lambda("x",Lambda("y",IsZero(If(Var("x"),Zero,Pred(Var("y"))))));
   LambdaA("x",Bool,If(Var("x"),Var("x"),Var("x")));
   LambdaA("x",Fun(Nat,Bool),Apply(Var("x"),Zero));
   LambdaA("x",Bool,Lambda("y",If(Var("x"),Var("y"),Zero)));
   LambdaA("x",Bool,Lambda("y",If(Var("y"),Var("x"),Zero)));
   Apply(Lambda("x",Var("x")),Lambda("y", Var("y")));
   Apply(Apply(Lambda("w",Var("w")),Lambda("x",Var("x"))),Apply(Lambda("y",Var("y")),Lambda("z",Var("z"))));
  ] ;;

List.iter (fun (e) ->
    print_string " " ;
    print_endline " " ; print_string "--" ; print_endline " " ;

    print_endline " " ; print_string "EXPRESSION" ; print_endline " " ;
    print_endline ([%show : exp] e) ;

    let v = infer (StringMap.empty) e (TermPairSet.empty) in

    match v with
    | Val(t,c) ->
      print_endline " " ; print_string "CONSTRAINT SET" ; print_endline " " ;
      print_endline ([%show : constr] c);
      let u = unify c (TermPairSet.empty) in
      begin match u with
        | Val(c1,c2) ->
          print_endline " " ; print_string "SOLUTION" ; print_endline " " ;
          print_endline ([%show : constr] c2) ;
          print_endline " " ; print_string "TYPE" ; print_endline " " ;
          let t' = solvetype c t in
          print_endline ([%show : ty] t')
        | _ ->
          print_endline " " ; print_string "SOLUTION" ; print_endline " " ;
          print_endline "CANNOT BE UNIFIED" ;
          print_endline " " ; print_string "TYPE" ; print_endline " " ;
          print_endline "UNTYPEABLE" ;
      end
    | _ ->
      print_endline " " ; print_string "CONSTRAINT SET" ; print_endline " " ;
      print_endline "CONSTRAINT SET ERROR" ;


  ) tests


(* Name: <William H Slocum> *)
