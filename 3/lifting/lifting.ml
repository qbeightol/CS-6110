
open Util
open Ast
open State

let rec is_target_exp (e : exp) : bool =
  match e with
    | Var _ -> true
    | Fun  _ -> false
    | Let _ -> false
    | Letrec _ -> false
    | App (e1, e2) -> is_target_exp e1 && is_target_exp e2
    | Cond (b, e1, e2) -> is_target_exp b && is_target_exp e1 && is_target_exp e2
    | Num _ -> true
    | Plus (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Minus (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Times (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Div (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Mod (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | And (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Or (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Not b -> is_target_exp b
    | Eq (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Leq (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Lt (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Bool _ -> true

let rec is_target_fun (e : exp) : bool =
  match e with
    | Fun(_,(Fun _ as e1)) -> is_target_fun e1
    | Fun(_,e1) -> is_target_exp e1
    | _ -> false

let rec is_target_prog (e : exp) : bool =
  match e with
    | Let(_,e1,p2) -> is_target_fun e1 && is_target_prog p2
    | Letrec(_,e1,p2) -> is_target_fun e1 && is_target_prog p2
    | _ -> is_target_exp e

let convert (e : exp) (s : state) : exp * state =
  let e_vars = allv e in
  let var_gen = Fresh.make e_vars in
  let rec convert e s0 =
    match e with
    | Let ([x], e1, e2) ->
      convert e1 s0  |> fun (e1', s1) ->
      convert e2 s1 |> fun (e2', s2) ->
      let f = Fresh.next var_gen in
      let e2_fvs = HashSet.values (fv e2) in
      let s3 = update s2 f (Closure (Fun (x::e2_fvs, e2'), State.make ())) in
      (App (Var f, e1'), s3)
    (* | Let (f::args, body, e) ->  *)
    | App (e1, e2) -> convert e s |>
      fun (e', s') -> convert e2 s'
    | _ -> failwith "Implement me!"
  in convert e s

let rec to_expr bs e = match bs with
  | [] -> e
  | (x, (Closure (Fun _ as f, s)))::tl ->
    if bindings s == [] then Let ([x], f, to_expr tl e)
    else Letrec ([x], f, to_expr tl e)
  | _ -> failwith "bindings held something other than a closure"

(* let rec to_expr bindings e = match bindings with
  | [] -> e
  | (x, (Closure (Fun (ys, body), _)))::tl when List.mem x ys ->
    (* x is bound to a recursive function; remove the argument corresponding to
     * it's own name *)
    let ys' =  List.filter ((<>) x) ys in
    Letrec ([x], (Fun (ys', body)), to_expr tl e)
  | (x, v)::tl -> Let ([x], v, to_expr tl e) *)

(* let rec to_expr bindings e = match bindings with
  | [] -> e
  | (x, v)::tl -> Let  *)

let lift (e : exp) : exp =
  let (e', state) = convert e (make ()) in
  let bs = bindings state in
  to_expr bs e'

(* let lift _ = failwith "!" *)
