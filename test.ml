open OUnit2
open Avl
open Expr
open ExtLib

(* A helper for testing primitive values (won't print datatypes well) *)
let t_any name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:dump);;

(* Feel free to add any new testing functions you may need *)




let a_tree = Node(0, "a", 5, Leaf, Leaf);;
let b_tree = Node (1, "x", 5, Leaf, Node (1, "y", 7, Leaf, Leaf));;
(* It can be useful to aggregate tests into lists if they test separate
functions, and put them together at the end *)

let get_tests = [
  t_any "get1" (get a_tree "a") (Some(5));
  t_any "get2" (get (Node(0, "b", 15, a_tree, Leaf)) "a") (Some(5));
  t_any "get3" (get (Node(0, "b", 15, a_tree, Leaf)) "c") None;
];;

let contains_tests = [
  t_any "contains1" (contains a_tree "c") false;
];;

let evaluate_tests = [
 t_any "evaluate1" (evaluate (Times(Num(0), Num(5))) Leaf) 0;
 t_any "evaluate2" (evaluate (Times(Plus(Variable("x"), Variable("y")), Num(5))) b_tree) 60; 
]

let all_tests =
  get_tests @
  contains_tests @
  evaluate_tests
  (* Your additional tests go here *)
;;

let suite = "suite">:::all_tests;;

run_test_tt_main suite

