open Expr;;
open Evaluation;;
(*module SS = Set.Make (struct
                      type t = Expr.varid
                      let compare = String.compare
  end );;
type varidset = SS.t ;; *)
let abs_expr1 = Let("f", Fun("x", Binop(Plus, Var("x"), Var("x"))), 
                App(Var("f"), App(Var("f"), Num(3))));; 
let abs_string_expr1 = "Let(f, Fun(x, Binop(+, Var(x), Var(x))), App(Var(f), App(Var(f), Num(3))))";;
let input2 = "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in f 4 ;;";;
let abs_expr2 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"),
                Num(0)), Num(1), Binop(Times, Var("x"), App(Var("f"),
                Binop(Minus, Var("x"), Num(1)))))),App(Var("f"), Num(4))) ;;
let abs_string_expr2 = "Letrec(f, Fun(x, Conditional(Binop(=, Var(x), Num(0)), Num(1), Binop(*, Var(x), App(Var(f), Binop(-, Var(x), Num(1)))))), App(Var(f), Num(4)))";;
let abs_expr3 = Fun("y", App(Var("f"), Binop(Plus, Var("x"), Var("y"))));;
let abs_expr4 = Let("double", Fun("x", Binop(Plus, Var("x"), Var("x"))),
                App(Var("double"), Num(4)));;
let abs_expr5 = Letrec("f", Fun("n", Conditional(Binop(Equals, Var("n"),
               Num(0)), Num(1), Binop( Times, Var("n"), App(Var("f"), 
               Binop(Minus, Var("n"), Num(1)))))), App(Var("f"), Num(3))) ;;
let abs_expr6 =  Fun("n", Conditional(Binop(Equals, Var("n"),
               Num(0)), Num(1), Binop( Times, Var("n"), App(Var("f"), 
               Binop(Minus, Var("n"), Num(1))))));;
let abs_expr7 = Let("f", Fun("x", Binop(Plus, Var("x"), Var("x"))),
                App(App(App(Var("f"),Var("f")), Var("f")), Num(3))) ;;
let abs_expr8 = Letrec("f", Fun("n", Conditional(Binop(Equals, Var("n"),
                Num(10)), Var("n"), Binop(Plus, Var("n"), App(Var("f"),
                Binop(Plus, Var("n"), Num(1)))))), App(Var("f"), Num(0))) ;; 
let abs_expr9 = Let("x", Var("y"), Let("y", Num(3), Binop(Plus, Var("x"), 
                Var("x"))));;
let abs_expr10 = Let("y", Num(3), Binop(Plus, Var("x"), 
                Var("x")));;
let test_exp_to_abstract_string () =
  assert(exp_to_abstract_string abs_expr1 = abs_string_expr1);
  assert(exp_to_abstract_string abs_expr2 = abs_string_expr2);;

let test_free_vars () =
  assert(free_vars abs_expr1 = vars_of_list []);
  assert(free_vars abs_expr2 = vars_of_list []);
  assert(free_vars abs_expr3 = vars_of_list ["f"; "x"]);
  assert(free_vars abs_expr4 = vars_of_list []);
  assert(free_vars abs_expr6 = vars_of_list ["f"]);
  assert(free_vars abs_expr10 = vars_of_list ["x"]);;
let test_subst () =
  assert(subst "x" (Var "z") abs_expr3 = 
   Fun("y", App(Var("f"), Binop(Plus, Var("z"), Var("y")))));
  assert(subst "x" (Binop(Plus, Var "u", Var "v")) abs_expr3 = 
   Fun("y", App(Var("f"), Binop(Plus, Binop(Plus, Var "u", Var "v"),
   Var("y")))));
  assert(subst "f" (Fun("u", Binop(Plus, Var "u", Var "u"))) abs_expr3 = 
         Fun("y", App((Fun("u", Binop(Plus, Var "u", Var "u"))),
         Binop(Plus, Var("x"), Var("y")))));
  (*print_string (exp_to_abstract_string (subst "x" (Num 3) abs_expr4)); *)
  assert(subst "x" (Num 3) abs_expr4 = abs_expr4);;
  (*print_string (exp_to_abstract_string (subst "x" (Var "y") abs_expr10));
  assert(subst "x" (Var "y") abs_expr10 = Let("x0", Num(3), Binop(Plus, 
         Var "x0", Var "x0")));
*)
let test_eval_s () =
  assert(eval_s abs_expr1 [] = Num 12);
  assert(eval_s abs_expr4 [] = Num 8);
  (*assert(eval_s abs_expr7 [] = Num 24); *)
  assert(eval_s abs_expr5 [] = Num 6);
  assert(eval_s abs_expr8 [] = Num 55);
  assert(eval_s abs_expr9 [] = Num 6);;

let test_eval_d () =
  assert(eval_d abs_expr1 (Env.create()) = Num 12);
  assert(eval_d abs_expr4 (Env.create()) = Num 8);
(*  assert(eval_d abs_expr7 (Env.create()) = Num 24); *)
  assert(eval_d abs_expr5 (Env.create()) = Num 6);
  assert(eval_d abs_expr8 (Env.create()) = Num 55);
  assert(eval_d abs_expr9 (Env.create()) = Num 6);;

let test_eval_l () =
  assert(eval_d abs_expr1 (Env.create()) = Num 12);
  assert(eval_d abs_expr4 (Env.create()) = Num 8);
(*  assert(eval_d abs_expr7 (Env.create()) = Num 24); *)
  assert(eval_d abs_expr5 (Env.create()) = Num 6);
  assert(eval_d abs_expr8 (Env.create()) = Num 55);
  assert(eval_d abs_expr9 (Env.create()) = Num 6);;



let run_tests () =
  test_exp_to_abstract_string () ;
  test_free_vars (); 
  test_subst ();
  test_eval_s();
  test_eval_d();
  test_eval_l();; 
run_tests();;
print_string "all tests passed \n";;
