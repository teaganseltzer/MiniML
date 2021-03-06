(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
                             Spring 2017
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)
    
open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime, generated by an explicit "raise" construct *)
exception EvalException ;;


(* Environments and values *)

module type Env_type = sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value =
      Closure(exp, env)

    (* Looks up the value of a variable in the environment *)
    let lookup (env : env) (varname : varid) : value =
      try  
        let (_, valref) = List.find (fun (id, _) -> id = varname) env in
        !valref
      with
      Not_found -> raise (EvalError ("unbound variable " ^ varname)) 
    
    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      try 
        let _ = lookup env varname in
        List.map (fun (id, valref) -> 
            if id = varname then id, loc else id, valref) env
      with
        EvalError _ -> (varname, loc) :: env  

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let rec value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with 
      | Closure(exp, env) -> 
          if printenvp then
            "value: " ^ (exp_to_string exp) ^ "; [ " ^ (List.fold_right 
            (fun (id, valref) c -> id ^ (value_to_string !valref) ^ c ) 
            env "") ^ "]"
          else exp_to_string exp 
      | Val e -> exp_to_string e
   
   (* Returns a printable string representation of an environment
    * Note on design, this reuses code in value_to_string. I would use
    * a helper function to reuse code but value_to_string would not be
    * in the scope of a helper function defined before value_to_string. *)
   let env_to_string (env : env) : string =
     "[" ^ (List.fold_right (fun (id, valref) c -> "( " ^ id ^ ") ; ( " ^
     (value_to_string !valref ^ ")") ^ c ) env "")

  end
;;
  
(* The evaluation function: Returns the result of type `value` of
   evaluating the expression `exp` in the environment `env`. In this
   initial implementation, we just convert the expression unchanged to
   a value and return it. *)


(** The external evaluator, which can be either the identity function,
    the substitution model version or the dynamic or lexical
    environment model version. *)
let eval_t exp _env = exp ;;

(*helper function to evaluate binops *)
  let eval_binop (b : binop) (e1 : expr) (e2 : expr) : expr =
    match b, e1, e2 with
    | Plus, Num n1, Num n2 -> Num (n1 + n2)
    | Minus, Num n1, Num n2 -> Num (n1 - n2)
    | Times, Num n1, Num n2 -> Num (n1 * n2)
    | Equals, e1, e2 -> Bool (e1 = e2)
    | LessThan, e1, e2 -> Bool (e1 < e2)
    | _ -> raise (EvalError "Binop used on incorrect types");;

let rec eval_s exp env =
  match exp with 
  | Var _ -> raise (EvalError "unbound variable")
  | Bool b -> Bool b
  (*currently do not need to worry about what the unop is as we only have 1 *)
  | Unop(_, e) -> 
      (match (eval_s e env) with
      | Num n -> Num (~- n)
      | _ -> raise (EvalError "attempted to negate a non-integer") )
  | Binop(b, e1, e2) -> eval_binop b (eval_s e1 env) (eval_s e2 env) 
  | Conditional(e1, e2, e3) -> 
      (match eval_s e1 env with 
      | Bool true -> eval_s e2 env
      | Bool false -> eval_s e3 env
      | _ -> raise (EvalError ": is not of type bool ")) 
  (*if we find a unapplied function we should just reuturn it as utop does*)
  | Fun(id, e) -> Fun(id, e)
  | Let(id, e1, e2)-> 
      (*first case covers aliasing, i.e let x = y in let y = ... *)
      (match e2 with 
      | Let(id2, def, body) -> 
          if id = id2 then eval_s (Let(id, def, body)) env
          else eval_s (Let(id2, subst id e1 def, subst id e1 body)) env
      | _ -> eval_s (subst id e1 e2) env)
  | Letrec(id, recfun, e2) -> 
      let (recfun_id, recfun_body) = 
        (match recfun with
        | Fun(recfun_id, recfun_body) -> recfun_id, recfun_body
        | _ -> raise (EvalError "cannot use letrec with a non function")) in
      let newvar = new_varname () in
      let newrecfun = Fun(newvar, subst recfun_id (Var newvar) recfun_body) in
      let newrec = subst id (Letrec(id, newrecfun, Var id)) recfun in
      eval_s (subst id newrec e2) env
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "Unassigned variable")
  | App (f, e1) ->
      (match eval_s f env, eval_s e1 env with
      | (Fun(id, body), e) -> 
          eval_s (subst id (eval_s e env) body ) env
      | _ -> raise (EvalError "this is not a function it cannot be applied"))
  | Num n -> Num n ;; 

(*helper function to find find value of var in env *)
let replace id env : expr = 
  match Env.lookup env id with
  | Env.Val(expr) -> expr
  | Env.Closure(expr, _) -> expr ;;

let rec eval_d (exp : expr) (env : Env.env) : expr =
  match exp with 
  | Var id -> eval_d (replace id env) env
  | Bool b -> Bool b
  | Unop(_, e) -> 
      (match (eval_d e env) with
      | Num n -> Num (~- n)
      | _ -> raise (EvalError "attempted to negate a non-integer"))
  | Binop(b, e1, e2) -> eval_binop b (eval_d e1 env) (eval_d e2 env) 
  | Conditional(e1, e2, e3) ->
      (match eval_d e1 env with 
      | Bool true -> eval_d e2 env
      | Bool false -> eval_d e3 env
      | _ -> raise (EvalError (exp_to_string e1 ^": is not of type bool "))) 
  | Fun(id, e) -> Fun(id, e)
  | Let(id, e1, e2)-> eval_d e2 (Env.extend env id (ref (Env.Val e1))) 
  | Letrec(id, recfun, e2) ->
      let eval_recfun = (eval_d recfun (Env.extend env id (ref 
                        (Env.Val Unassigned)))) in
      eval_d e2 (Env.extend env id (ref (Env.Val eval_recfun)))
  | Raise -> raise EvalException
  | Unassigned -> raise (EvalError "Unassigned variable")
  | App(f, e1) ->
      (match eval_d f env, eval_d e1 env with
       | (Fun(id, body), e ) ->
            eval_d body (Env.extend env id (ref (Env.Val e)))  
       | _ -> raise (EvalError "this is not a function it cannot be applied"))
  | Num n -> Num n;;

(* in eval_l we want to work with values instead of exprs, so we need to do
  * most of our evaluation inside a helper function and then match the 
  * result to return an expr for minimml.ml to display*)
let rec eval_l (exp : expr) (env : Env.env) : expr = 
  let rec heval_l (inval : Env.value) (env : Env.env) : Env.value =   
    let exp =
      match inval with 
      | Env.Val e -> e
      | Env.Closure(e, _) -> e in
    match exp with 
    | Var id -> heval_l (Env.Val( replace id env)) env
    | Bool b -> (Env.Val (Bool b))
    | Unop(_, e) -> 
        (match (heval_l (Env.Val e) env) with
        | Env.Val(Num n) -> (Env.Val (Num (~- n)))
        | _ -> raise (EvalError "attempted to negate a non-integer"))
    | Binop(b, e1, e2) ->
        (match (heval_l (Env.Val e1) env), (heval_l (Env.Val e2) env) with
        | Env.Val e1, Env.Val e2 ->
             Env.Val (eval_binop b e1 e2)
        | _ -> raise (EvalError "Incompatabile types for binop"))
    | Conditional(e1, e2, e3) ->
        (match heval_l (Env.Val e1) env with 
        | Env.Val(Bool true) -> heval_l (Env.Val e2) env
        | Env.Val(Bool false) -> heval_l (Env.Val e3) env
        | _ -> raise (EvalError (exp_to_string e1 ^": is not of type bool ")))
    | Fun(id, e) -> (Env.close (Fun(id, e)) env)
    | Let(id, e1, e2)->
         heval_l (Env.Val e2) (Env.extend env id (ref (Env.close e1 env)))  
    | Letrec(id, recfun, e2) ->
        let eval_recfun = (eval_l recfun (Env.extend env id
                          (ref (Env.Val Unassigned)))) in
       (Env.Val (eval_l e2  (Env.extend env id (ref (Env.Val eval_recfun)))))
    | Raise -> raise EvalException
    | Unassigned -> raise (EvalError "Unassigned variable")
    | App(e1, e2) ->
        (match heval_l (Env.Val e1) env with
        | Env.Closure(Fun(id, body), c_env ) ->
            heval_l (Env.Val body) (Env.extend c_env id 
            (ref (heval_l (Env.Val e2) env)))
        | _ -> raise (EvalError "this is not a function it cannot be applied"))
    | Num n -> (Env.Val(Num n)) in
  match heval_l (Env.Val exp) env with
  | Env.Val exp -> exp
  | Env.Closure (exp, _) -> exp;;
   
let evaluate = eval_l ;;
