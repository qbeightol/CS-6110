module Fresh = struct
    module Counter = struct
        let value = ref 0
        let get () = let result = !value in incr value; result
    end
    let string () = "x" ^ string_of_int (Counter.get ())
end

module FL = struct
    type t =
        | Var of string
        | App of t * t
        | Fun of string * t
        | Str of string
        | Eq  of t * t
        | If  of t * t * t
        | B   of bool
        | Int of int
        | Add of t * t
        | Err

    let rec to_string = function
        | Var x -> "(Var " ^ x ^ ")"
        | App (l, r) -> "(App " ^ to_string l ^ " " ^ to_string r ^ ")"
        | Fun (x, body) -> "(Fun " ^ x ^ " " ^ to_string body ^ ")"
        | Str s -> "(Str " ^ s ^ ")"
        | Eq (l, r) -> "(= " ^ to_string l ^ " " ^ to_string r ^ ")"
        | If (b, t, f) -> "(if " ^ to_string b ^ " " ^ to_string t ^ " " ^ to_string f ^ ")"
        | B b -> string_of_bool b
        | Int n -> string_of_int n
        | Add (l, r) -> "(+ " ^ to_string l ^ " " ^ to_string r ^ ")"
        | Err -> "Err"

    let rec subst e v x =
        match e with
        | Var y when y = x         -> v
        | Var y (* o.w. *)         -> Var y
        | App (l, r)               -> App (subst l v x, subst r v x)
        | Fun (y, body) when y = x -> Fun (y, body)
        | Fun (y, body) (* o.w.*)  -> Fun (y, subst body v x)
        | Str s -> Str s
        | Eq (l, r) -> Eq (subst l v x, subst r v x)
        | If (b, t, f) -> If (subst b v x, subst t v x, subst f v x)
        | B b -> B b
        | Int n -> Int n
        | Add (l, r) -> Add (subst l v x, subst r v x)
        | Err -> Err

    let rec eval term =
        print_endline (to_string term);
        match term with
        | Var x      -> failwith ("unbound id: " ^ x)
        | App (l, r) ->
            begin

            match eval l with
            | Fun (x, body) ->
                begin
                match eval r with
                | Err -> Err
                | r'  -> eval (subst body r' x)
                end
            | Err           -> Err
            | _             -> failwith "not possible!"
            end
        | Fun (x, body) -> Fun (x, body)
        | Str s -> Str s
        | Eq (l, r) -> B (l = r)
        | If (b, t, f) ->
            begin
            match eval b with
            | B true -> eval t
            | B false -> eval f
            | Err -> Err
            | _ -> failwith "nonsensical guard"
            end
        | B b -> B b
        | Int n -> Int n
        | Add (l, r) ->
            begin
            match (eval l, eval r) with
            | Int m, Int n -> Int (m + n)
            | Err, _ -> Err
            | _, Err -> Err
            | _ -> failwith "nonsensical sub expressions"
            end
        | Err -> Err

    let empty = Fun ("_", Err)

    let lookup env x = App (env, x)

    let update env value x =
        let y = Fresh.string () in
        Fun (y, If (Eq (Var y, x), value, App (env, Var y)))
end

module Lambda = struct

    type t =
        | Var of string
        | App of t * t
        | Fun of string * t
        | Let of string * t * t
        | Add of t * t
        | Int of int


    let rec _translate_s term env =
        match term with
        | Var x         -> FL.lookup env (FL.Str x)
        | App (l, r)    -> FL.App (_translate_s l env, _translate_s r env)
        | Fun (x, body) ->
            let v = Fresh.string () in
            FL.Fun (v, _translate_s body (FL.update env (FL.Var v) (FL.Str x)))
        | Let (x, e1, e2) -> _translate_s (App (Fun (x, e2), e1)) env
        | Add (l, r) -> FL.Add (_translate_s l env, _translate_s r env)
        | Int n -> FL.Int n

    let rec _translate_d term env =
        match term with
        | Var x -> FL.lookup env (FL.Str x)
        | App (l, r) -> FL.App (FL.App (_translate_d l env, _translate_d r env), env)
        | Fun (x, body) ->
            let v = Fresh.string () in
            let tau = Fresh.string () in
            FL.Fun (v, FL.Fun (tau,
                _translate_d body (FL.update (FL.Var tau) (FL.Var v) (FL.Str x))
            ))
        | Let (x, e1, e2) -> _translate_d (App (Fun (x, e2), e1)) env
        | Add (l, r) -> FL.Add (_translate_d l env, _translate_d r env)
        | Int n -> FL.Int n

    let translate_s term = _translate_s term FL.empty

    let translate_d term = _translate_d term FL.empty

    let eval_s term = FL.eval (translate_s term)

    let eval_d term = FL.eval (translate_d term)


    let id = Fun ("x", Var "x")
    (* let x = translate_s (Let ("x", B true, Var "x")) *)
    let y = translate_s (Let ("y", id, Var "y"))
    (* let prompt_expr =  *)

    let prompt =
        Let ("x", Int 3,
        Let ("f", Fun ("y", Add (Var "x", Var "y")),
        Let ("x", Int 5,
        Let ("g", Fun ("z", Let ("x", App (Var "f", Var "x"), App (Var "f", Var "x"))),
        Let ("x", Int 7,
        Let ("y", Int 9,
        Add (App (Var "g", Var "y"), App (Var "f", Var "x"))
        ))))))
end
