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
      failwith "close not implemented" ;;

    (* Looks up the value of a variable in the environment *)
    let lookup (env : env) (varname : varid) : value =
      failwith "lookup not implemented" ;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      failwith "extend not implemented" ;;

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let value_to_string ?(printenvp : bool = true) (v : value) : string =
      failwith "value_to_string not implemented" ;;

   (* Returns a printable string representation of an environment *)
   let env_to_string (env : env) : string =
     failwith "env_to_string not implemented" ;;

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
(*everytime we run into a let add that var to the env and then replace
 * free variables  as we check if they are free*)
let rec eval_s exp env =
  let eval_binop (b : binop) (e1 : expr) (e2 : expr) : expr =
    match b, e1, e2 with
    | Plus, Num n1, Num n2 -> Num (n1 + n2)
    | Minus, Num n1, Num n2 -> Num (n1 - n2)
    | Times, Num n1, Num n2 -> Num (n1 * n2)
    | Equals, e1, e2 -> Bool (e1 = e2)
    | LessThan, e1, e2 -> Bool (e1 < e2)
    | _ -> raise (EvalError "Binop used on incorrect typs") in

  match exp with 
  | Var id -> Var id
  | Num n -> Num n
  | Bool b -> Bool b
  | Unop(u, e) -> 
      (match (eval_s e _) with
      | Num n -> Num (~- n)
      | _ -> raise (EvalError "attempted to negate a non-integer"))
  | Binop(b, e1, e2) -> eval_binop b e1 e2  
  failwith "eval_s not done";;
  
  let eval_d _ = failwith "eval_d not implemented" ;;
let eval_l _ = failwith "eval_l not implemented" ;;

let evaluate = eval_t ;;
