(* 
			 CS 51 Final Project
			MiniML -- Expressions
			     Spring 2017
*)

(* Abstract syntax of MiniML expressions *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;
      
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(* Sets of varids *)
module SS = Set.Make (struct
		       type t = varid
		       let compare = String.compare
		     end ) ;;

type varidset = SS.t ;;

(* Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(* Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;
  
(* Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  let rec all_vars (e : expr) (all : varidset) (used : varidset)
         : (varidset * varidset) =
    (match e with
    | Var id -> SS.add id all, used
    | Num _ -> all, used
    | Bool _ -> all, used
    | Unop (_, e1) -> all_vars e1 all used 
    | Binop (_, e1, e2) ->
        let lall, lused = all_vars e1 all used in
        let rall, rused = all_vars e2 all used in
        (SS.union lall rall), (SS.union lused rused) 
    | Conditional (e1, e2, e3) -> 
        let lall, lused = all_vars e1 all used in
        let mall, mused = all_vars e2 all used in
        let rall, rused = all_vars e3 all used in
        (SS.union (SS.union lall mall) rall),
        (SS.union (SS.union lused mused) rused) 
    | Fun (id, e1) -> all_vars e1 all (SS.add id used)
    | Let (id, e1, e2) -> 
        let nused = SS.add id used in
        let lall, lused = all_vars e1 all nused in
        let rall, rused = all_vars e2 all nused in
        (SS.union lall rall), (SS.union lused rused)
    | Letrec (id, e1, e2) -> 
        let nused = SS.add id used in
        let lall, lused = all_vars e1 all nused in
        let rall, rused = all_vars e2 all nused in
        (SS.union lall rall), (SS.union lused rused)
    | Raise -> all, used
    | Unassigned -> all, used
    | App (e1, e2) ->
        let lall, lused = all_vars e1 all used in
        let rall, rused = all_vars e2 all used in
        (SS.union lall rall), (SS.union lused rused)) in 
  let all, used = all_vars exp SS.empty SS.empty in
  SS.iter print_string all;
  all;;
(* Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;
  
(* Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;

    
(* exp_to_string -- Returns a string representation of the expr *)
let rec exp_to_string (exp : expr) : string =
(* let binop_to_string (b : binop) : string =
  match b with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<" in
*)
 failwith "exp_to_string not implemented" ;;

(* exp_to_abstract_string: Returns a string representation of the abstract
   syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  let binop_to_abs_string (b : binop) : string =
    match b with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Equals -> "="
    | LessThan -> "<" in

  (*using match statement so that this is easier to extend later *)
  let unop_to_abs_string (u : unop) : string =
    match u with
    | Negate -> "Negate" in

  match exp with
  | Var id -> "Var(" ^ id ^ ")"
  | Num x -> "Num(" ^ string_of_int x ^ ")"
  | Bool b -> "Bool (" ^ string_of_bool b ^ ")"
  | Unop (u, e) -> unop_to_abs_string u ^ "(" ^ exp_to_abstract_string e ^ ")"
  | Binop (b, e1, e2) -> "Binop" ^ "(" ^ binop_to_abs_string b ^ ", " ^
                        exp_to_abstract_string e1 ^ ", " ^
                        exp_to_abstract_string e2 ^ ")"
  | Conditional (e1, e2, e3) -> 
      "Conditional (" ^ exp_to_abstract_string e1 ^ ", " ^ 
       exp_to_abstract_string e2 ^ ", " ^ exp_to_abstract_string e3 ^ ")"
  | Fun (id, e1) -> "Fun(" ^ id ^ ", " ^ exp_to_abstract_string e1 ^")"
  | Let (id, e1, e2) -> 
      "Let(" ^ id ^ ", " ^ exp_to_abstract_string e1 ^ ", " 
      ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (id, e1, e2) ->
      "Letrec(" ^ id ^ ", " ^ exp_to_abstract_string e1 ^ ", " ^
      exp_to_abstract_string e2 ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (e1, e2) -> 
      "App(" ^ exp_to_abstract_string e1 ^ ", " ^ exp_to_abstract_string e2 
      ^ ")";; 

