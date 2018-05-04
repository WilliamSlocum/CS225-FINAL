(* Name: <William H Slocum> *)
(* Course: UVM CS 225 Spring 2018 - Darais *)
(* Final Project *)

open Util
open StringSetMap
open Final

(* Testing Suite for Final.ml *)

(* ====================================================================== *)

let tests : (exp) list =
  [If(IsZero(Zero),Pred(Succ(Zero)),Succ(Zero));
   IsZero(Apply(Lambda("X0", Var("X0")),Succ(Zero)));
   Apply(Lambda("x",Var("x")),Lambda("y", Var("y")));
   If(IsZero(Zero),Pred(Succ(Zero)),Succ(Zero));
   Lambda("x",Succ(Var("x")));
   Lambda("x",Succ(Var("x")));
   If(Pred(Zero),True,False);
   Lambda("x",If(Var("x"),False,False));
   Lambda("x",If(Var("x"),False,IsZero(False)));
   Apply(Apply(Lambda("w",Var("w")),Lambda("x",Var("x"))),Apply(Lambda("y",Var("y")),Lambda("z",Var("z"))));
   LambdaA("x",Bool,If(Var("x"),Var("x"),Var("x")));
   Lambda("x",IsZero(If(Var("x"),Zero,Pred(Zero))));
   If(True,Lambda("x",Var("x")),Lambda("x",Var("x")));
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
          print_endline ([%show : ty] t)
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
