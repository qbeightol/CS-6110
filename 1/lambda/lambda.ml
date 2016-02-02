(* An interpreter for the lambda calculus *)

open Printf
open Util
open Ast

(* translate translates from an expression of type expr_s
   containing syntactic sugars to an expression of type expr
   not containing any *)
let rec translate (e : expr_s) : expr =
  match e with
  | Var_s(x) -> failwith "Implement me!" 
  | Fun_s([], e) -> failwith "translate: Empty parameter list"
  | Fun_s([x], e) -> failwith "Implement me!" 
  | Fun_s(h::t, e) -> failwith "Implement me!" 
  | Let_s(x, e1, e2) -> failwith "Implement me!" 
  | App_s(e1, e2) -> failwith "Implement me!"

(* is_value determines whether the given expression is a value. A value    *)
(* is a fully evaluated expression.                                        *)
let is_value (e : expr) : bool =
  match e with
  | Fun _ -> true
  | Var x -> failwith ("Unbound variable " ^ x)
  | _ -> false

(* check if term is closed *)
let is_closed (e : expr) : bool = HashSet.size (fv e) = 0

(* substitute v for x in e, avoiding capture *)
let rec subst (e : expr) (v : expr) (x : id) : expr =
  match e with
  | Var y -> failwith "Implement me!"
  | Fun (y, e1) -> failwith "Implement me!" 
  | App (e1, e2) -> failwith "Implement me!" 

(* cbv_step e is the result with one rewrite step applied to e *)
let rec cbv_step (e : expr) : expr =
  match e with
  | Var x -> failwith ("Unbound variable " ^ x)
  | Fun _ -> failwith ("Already reduced")
  | App (Fun (x, e1), (Fun _ as e2)) -> failwith "Implement me!"
  | App (Fun (x, e1), e2) -> failwith "Implement me!" 
  | App (e1, e2) -> failwith "Implement me!"

(* Some examples to play with                     *)
(*
let id = fun x -> x in
let tt = fun x y -> x in
let ff = fun x y -> y in
let cond = fun x y z -> x y z in
let zero = ff in
let succ = fun n f x -> f (n f x) in
let one = succ zero in
let two = succ one in
let plus = fun n1 n2 -> n1 succ n2 in
let four = (plus two) two in
(plus four) four
*)
