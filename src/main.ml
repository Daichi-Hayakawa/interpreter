open Syntax
open Eval
(*open Unifier*)

let print_type = true

let rec read_eval_print env tyenv constraints =
  print_string "# ";
  flush stdout;
  try (
    let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    try (
      let (t, constraints_ret, newtyenv)  = Make_constraints.make_constraints_cmd tyenv cmd in
      let newconstraints = constraints_ret in
      (*let newconstraints = Make_constraints.union_constraints constraints constraints_ret in*)
      (
        try (
          let solver = Unifier.unify newconstraints in 
          if print_type then (
            print_string "type : ";
            Unifier.print_type (Unifier.ty_subst solver t);
            print_newline ()
          );
          let (id, newenv, v) = eval_command env cmd in
          (Printf.printf "%s = " id;
          print_value v;
          (*
          print_newline();
          print_command cmd;
          *)
          print_newline ();
          read_eval_print newenv newtyenv newconstraints)
        )
        with 
        | Unifier.TyError -> (
          print_string "type error occured !!\n";
          read_eval_print env tyenv constraints
        )
      )
    )
    with
    | Unifier.TyError -> (
      print_string "type error occured !!\n";
      read_eval_print env tyenv constraints
    )
    | Make_constraints.Unbound -> (
      print_string "Unbound Error !!\n";
      read_eval_print env tyenv constraints
    )
  )
  with
  | Parser.Error -> (
    print_string "syntax error occured !!\n";
    read_eval_print env tyenv constraints
  )
  
  (*
  print_command cmd;
  print_newline ();
  *)

let (initial_env:env) =
  empty_env

let initial_tyenv = []

let initial_constraints = []

let _ = read_eval_print initial_env initial_tyenv initial_constraints