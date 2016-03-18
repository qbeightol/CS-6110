
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
  let rec apply_to_all e xs = match xs with
    | []     -> e
    | hd::tl -> apply_to_all (App (e, Var hd)) tl
  in

  (* The output store ought to be a mapping from function names to closures
   * where the closure's environment is [] when f is function that should be
   * converted to a regular let binding, or env = [f ↦ (λ args. body, env)]
   * when f should be translated into a recursive let (i.e. it's environment
   * is the output of a call to rec_update. )
   *)
  let rec convert e s0 =
    match e with
    | Var _ -> (e, s0)
    | Fun (args, body) ->
      convert body s0 |> fun (body', s1) ->
      let e_fvs = HashSet.values (fv e) in
      let f = Fresh.next var_gen in
      let s2 = update s1 f (Closure (Fun (e_fvs@args, body'), State.make ())) in
      (apply_to_all (Var f) e_fvs, s2)
    | App (e0, e1) ->
      convert e0 s0 |> fun (e0', s1) ->
      convert e1 s1 |> fun (e1', s2) ->
      (App (e0', e1), s2)
    | Let ([], _, _) -> failwith "invalid let expression"
    | Let (f::args, e1, e2) ->
      convert (App (Fun ([f], e1), Fun(args, e2))) s0
    | Letrec ([], _, _) -> failwith "invalid let rec expression"
    | Letrec (f::args, e1, e2) ->
      let desugared_f_def = Fun (args, e1) in
      let new_args = HashSet.values (fv desugared_f_def) in
      let f_app_to_new_args = apply_to_all (Var f) new_args in
      (* the desugared_f_body with f new_args subsituted in for f (so that
       * recursive calls to f work). *)
      let fixed_desugard_f_def = subst f_app_to_new_args f desugared_f_def in
      let s0_with_f_bound_to_err = update s0 f Error in
      let final_f_def, s1 = convert fixed_desugard_f_def s0_with_f_bound_to_err in
      let final_f = Fun (new_args, final_f_def) in
      let (_, final_f_state) = rec_update (Closure (final_f, State.update s1 f Error)) in
      convert e2 final_f_state
    | Cond (b, e1, e2) ->
      let (b', s1) = convert b s0 in
      let (e1', s2) = convert e1 s1 in
      let (e2', s3) = convert e2 s2 in
      Cond (b', e1', e2'), s3
    | Num n  -> Num n, s0
    (* warning: this is really dumb *)
    | Plus (l, r)  ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Plus (l', r'), s2
    | Minus (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Minus (l', r'), s2
    | Times (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Times (l', r'), s2
    | Div (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Div (l', r'), s2
    | Mod (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Mod (l', r'), s2
    | And (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      And (l', r'), s2
    | Or (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Or (l', r'), s2
    | Not b -> Not b, s0
    | Eq (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Eq (l', r'), s2
    | Leq (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Leq (l', r'), s2
    | Lt (l, r) ->
      let (l', s1) = convert l s0 in
      let (r', s2) = convert r s1 in
      Lt (l', r'), s2
    | Bool b -> Bool b, s0
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
