Most of this Project is accomplished with two Functions: Infer and Unify. Infer is seemingly finished, but more testing is required. Unify is commented-out at the moment. I must still convert the Unify Pseudo-Code into Executable OCaml Code.

To Build and Run this Code, use the command "make Final". As of now, Final.ml will output three Test Cases. The Output is of the form (Type, ConstraintSet). Type is the Type of Well-Typed Expressions, but it isn't particularly meaningful for Expressions that aren't Well-Typed. The Constraint Set is a list of Constraints that must be satisfied for the Expression to be Well-Typed.

1) Our first Test Case demonstrates a Well-Typed Expression. Notice that the Constraint Set is {(Bool = Bool),(Nat = Nat)}. Since these conditions are obviously True, the Expression is Well-Typed.

2) Our second Test Case demonstrates an Expression that is not Well-Typed. Notice the Constraint Set contains (Bool = Nat). No Substitution can satisfy this Constraint.

3) Our third Test Case isn't immediately apparent whether the Expression is Well-Typed. It contains Constraints with Type Variables such as (X1 = Nat). To determine a Solution to this Constraint Set, we will need to use the Unify Function. Doing so would find the Substitutions (X1 -> Nat) and (X2 -> Nat). By inspecting the Expression,  we can interpret that as "the Expression is Well-Typed when the Lambda Variable has Type Nat".

In some cases, Constraint Sets have multiple Solutions. Unify will find the "most simple" Solution. 

To test the accuracy of Unify, we can use the Type Inference Function from Hw5 to see if the Expression is Well-Typed under the Substitution Solution.