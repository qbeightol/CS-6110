(* An interpreter for the lambda calculus *)

open Printf
open Util
open Ast

(* translate translates from an expression of type expr_s
   containing syntactic sugars to an expression of type expr
   not containing any *)
let rec translate (e : expr_s) : expr =
  match e with
  | Var_s x               -> Var x
  | Fun_s ([], e_s)       -> failwith "translate: Empty parameter list"
  | Fun_s ([x], e_s)      -> Fun (x, translate e_s)
  | Fun_s (h::t, e_s)     -> Fun (h, translate (Fun_s (t, e_s)))
  | Let_s (x, e1_s, e2_s) -> App (Fun (x, translate e2_s), translate e1_s)
  | App_s (e1_s, e2_s)    -> App (translate e1_s, translate e2_s)

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
let rec subst e v x = match e with
  | Var y when y = x    -> v
  | Var _   (* e ≠ x *) -> e
  | Fun (y, _) when y = x -> e
  | Fun (y, e0) when not @@ HashSet.mem (fv v) y -> Fun (y, subst e0 v x)
  | Fun (y, e0) ->
    let expr_of_unsafe_ids = App (App (e0, v), App (Var x, Var y)) in
    let z = fresh expr_of_unsafe_ids in
    Fun (z, subst (subst e (Var z) y) e x)
  | App (e1, e2)              -> App (subst e1 v x, subst e2 v x)

(* cbv_step e is the result with one rewrite step applied to e *)
let rec cbv_step (e : expr) : expr =
  match e with
  | Var x -> failwith ("Unbound variable " ^ x)
  | Fun _ -> failwith ("Already reduced")
  | App (Fun (x, e1), (Fun _ as e2)) -> subst e1 e2 x
  | App (Fun (x, e1), e2) -> App (Fun (x, e1), cbv_step e2)
  | App (e1, e2) -> App (cbv_step e1, e2)

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

(* other tests
let t = fun x y -> x in
let f = fun x y -> y in
let cond = fun b e1 e2 -> b e1 e2 in
cond t (fun a -> a) (fun b -> b)

let pair = fun a b proj -> proj a b in
let fst = fun pair -> pair (fun a b -> a) in
let snd = fun pair -> pair (fun a b -> b) in
let xy = pair (fun x -> x) (fun y -> y) in
fst (pair (snd xy) (fst xy))

(fun x y -> x) (fun a -> y)

*)
