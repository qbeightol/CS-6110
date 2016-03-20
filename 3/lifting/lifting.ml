
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
      let e_fvs = HashSet.values (fv (Fun (args, body'))) in
      let f = Fresh.next var_gen in
      let s2 = update s1 f (Closure (Fun (e_fvs@args, body'), State.make ())) in
      (apply_to_all (Var f) e_fvs, s2)
    | App (e0, e1) ->
      convert e0 s0 |> fun (e0', s1) ->
      convert e1 s1 |> fun (e1', s2) ->
      (App (e0', e1'), s2)
    | Let ([], _, _) -> failwith "invalid let expression"
    (* making this a special case doesn't seem necessary, but the output code
     * is more readable, and less likely to cause problems during evaluation *)
    | Let ([f], e1, e2) ->
      convert (App (Fun ([f], e2), e1)) s0
    | Let (f::args, e1, e2) ->
      convert (App (Fun ([f], e2), Fun(args, e1))) s0
    | Letrec ([], _, _) -> failwith "invalid let rec expression"
    | Letrec (f::args, e1, e2) ->

      let f_fvs = fv (Fun (f::args, e1)) in
      let new_args = HashSet.values f_fvs in
      let new_f_name = Fresh.next var_gen in
      let new_f_app_to_new_args = apply_to_all (Var new_f_name) (new_args) in
      let e1_with_fixed_f_calls = subst new_f_app_to_new_args f e1 in
      (* print_endline (State.state_to_string s0); *)
      let final_e1, s1 = convert e1_with_fixed_f_calls s0 in
      (* print_endline (State.state_to_string s1); *)
      let new_f_def = Fun(new_args@args, final_e1) in
      (* let new_f_def, s2 = convert (Fun (new_args@args, final_e1)) s1 in *)
      (* print_endline (Ast.to_string new_f_def);
      print_endline (State.state_to_string s2); *)
      (* let new_f_expr = Fun (new_args@args, new_f_def) in *)
      let (_, final_state) = rec_update (Closure (new_f_def, s1)) new_f_name in
      (* let (_, final_state) = rec_update (Closure (Fun (args, apply_to_all new_f_def args), s1)) new_f_name in *)
      let e2_with_updated_f_calls = subst new_f_app_to_new_args f e2 in
      convert e2_with_updated_f_calls final_state
(*

      let final_e1, s1 = convert e1_with_fixed_f_calls in


      let e1', s1 = convert (Fun (args, e1)) in
      let e1_fvs = fv e1 in
      let e1'_fvs = fv e1' in
      let e1'_new_fvs = HashSet.

      let desugared_f_def = Fun (args, e1) in
      let desugared_f_def_fvs = fv desugared_f_def in
      HashSet.remove desugared_f_def_fvs f;
      let new_args = HashSet.values desugared_f_def_fvs in
      let new_f_name = Fresh.next var_gen in
      let new_f_app_to_new_args = apply_to_all (Var new_f_name) new_args in
      (* the desugared_f_body with f new_args subsituted in for f (so that
       * recursive calls to f work). *)
      let fixed_desugard_f_def = subst new_f_app_to_new_args f desugared_f_def in
      let final_f_def, s2 = convert fixed_desugard_f_def s1 in
      let final_f = Fun (new_args, final_f_def) in
      let (_, final_f_state) = rec_update (Closure (final_f, s2)) new_f_name in
      let e2_with_updated_f_calls = subst new_f_app_to_new_args f e2 in
      convert e2_with_updated_f_calls final_f_state *)

      (* let desugared_f_def = Fun (args, e1) in
      let desugared_f_def_fvs = fv desugared_f_def in
      HashSet.remove desugared_f_def_fvs f;
      let new_args = HashSet.values desugared_f_def_fvs in
      let new_f_name = Fresh.next var_gen in
      let new_f_app_to_new_args = apply_to_all (Var new_f_name) new_args in
      (* the desugared_f_body with f new_args subsituted in for f (so that
       * recursive calls to f work). *)
      let fixed_desugard_f_def = subst new_f_app_to_new_args f desugared_f_def in
      let final_f_def, s1 = convert fixed_desugard_f_def s0 in
      let final_f = Fun (new_args, final_f_def) in
      let (_, final_f_state) = rec_update (Closure (final_f, s1)) new_f_name in
      let e2_with_updated_f_calls = subst new_f_app_to_new_args f e2 in
      convert e2_with_updated_f_calls final_f_state *)
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
    if bindings s == [] then to_expr tl (Let ([x], f, e))
    else to_expr tl (Letrec ([x], f, e))
  | (x, v)::_ ->
    failwith ("bindings held something other than a function\n"
              ^ State.state_to_string (State.of_bindings bs))

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
