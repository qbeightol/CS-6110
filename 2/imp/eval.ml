
open Ast
open State

(* Evaluation according to big-step semantics *)

(* evaluate an arithmetic expression in a state *)
let rec eval_a (a : aexp) (s : state) : int =
  match a with
  | Var x        -> lookup s x
  | Number n     -> n
  | Plus (l, r)  -> eval_a l s  + eval_a r s
  | Minus (l, r) -> eval_a l s - eval_a r s
  | Times (l, r) -> eval_a l s * eval_a r s
  | Div (l, r)   -> eval_a l s / eval_a r s
  | Mod (l, r)   -> eval_a l s mod eval_a r s
  | Input        -> read_int ()

(* evaluate a boolean expression in a state *)
let rec eval_b (b : bexp) (s : state) : bool =
  match b with
  | Eq (l, r)  -> eval_a l s = eval_a r s
  | Leq (l, r) -> eval_a l s <= eval_a r s
  | Lt (l, r)  -> eval_a l s < eval_a r s
  | Not b      -> not (eval_b b s)
  | And (l, r) -> eval_b l s || eval_b r s
  | Or (l, r)  -> eval_b l s || eval_b r s
  | True       -> true
  | False      -> false

(* evaluate a command in a state *)
let rec eval_c (c : com) (s : state) : state =
  match c with
  | While (b, c)     -> if eval_b b s then eval_c (Comp (c, While (b, c))) s else s
  | Cond (b, ct, cf) -> if eval_b b s then eval_c ct s else eval_c cf s
  | Comp (c1, c2)    -> eval_c c1 s |> fun s' -> eval_c c2 s'
  | Assg (x, a)      -> rebind s x (eval_a a s)
  | Print a          -> print_int (eval_a a s); s
  | Skip             -> s
