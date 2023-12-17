open Syntax

exception Unbound
exception Match_failure
exception EvalErr


let empty_env = []
let extend x (t:thunk) env = (x, t) :: env

(*
let lookup x (env:(name * thunk) list) : env =
  try List.assoc x env with Not_found -> raise Unbound
*)

let rec lookup x (env:(name * thunk) list) : thunk =
match env with
| [] -> raise Unbound
| (n, t) :: xs -> if (n = x) then t else lookup x xs

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    (try
       let Thunk(e', env') = lookup x env in
       eval_expr env' e'
     with
     | Unbound -> raise EvalErr)
  | EFun (x, e) -> VFun (x, e, env)
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> raise EvalErr)
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise EvalErr)
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise EvalErr)
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 / i2)
     | _ -> raise EvalErr)
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | _ -> raise EvalErr)
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | _ -> raise EvalErr)
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then (
         let v2 = eval_expr env e2 in v2
       ) else (
         let v3 = eval_expr env e3 in v3
       )
     | _ -> raise EvalErr)
  | ELet (n, e1, e2) ->
    let local_env = extend n (Thunk(e1, env)) env in (* サンクを追加 *)
    let v2 = eval_expr local_env e2 in
    v2
  | ELetRec (f, x, e1, e2) ->
    let env' = extend f (Thunk(e1, env)) env in (* サンクを追加 *)
    eval_expr env' e2
  | EAnd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match (v1, v2) with
    | (VBool b1, VBool b2) -> VBool(b1 && b2)
    | _ -> raise EvalErr)
  | EOr (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match (v1, v2) with
    | (VBool b1, VBool b2) -> VBool(b1 || b2)
    | _ -> raise EvalErr)
  | EApp (e1, e2) ->
    let v1 = eval_expr env e1 in
    let local_thunk = Thunk (e2, env) in(* ここではまだ評価しない *)
    (match v1 with
      | VFun    (x, e, oenv) -> 
        eval_expr (extend x local_thunk oenv) e
      | VRecFun (f, x, e, oenv) ->
        (*print_string "VRecFun reach\n";*)
        let env' = (extend x local_thunk oenv) in
        (*print_string "VRecFun after\n";*)
        eval_expr env' e
      | _ -> raise EvalErr)
  | EPair (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VPair (v1, v2)
  | ENil -> VNil
  | ECons (e1, e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    VCons (v1, v2)


let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (n, e) -> 
    let v = eval_expr env e in
    ("val "^n, extend n (Thunk(e, env)) env, v) (* サンクを追加 *)
  | CRecDecl (f, x, e) ->
    let v = VRecFun(f, x, e, env) in
    ("val "^f, extend f (Thunk(e, env)) env, v) (* サンクを追加 *)
  | CDeclseq (cdecl, rest) ->
    let (n, new_env, v) = eval_command env cdecl in
    Printf.printf "%s = " n;
    print_value v;
    print_newline();
    eval_command new_env rest
