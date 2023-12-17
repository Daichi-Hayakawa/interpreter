type name = string

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetRec    of name * name * expr * expr
  | EAnd       of expr * expr
  | EOr        of expr * expr
  | EFun       of name * expr
  | EApp       of expr * expr
  | EPair      of expr * expr
  | ENil
  | ECons      of expr * expr

type command =
  | CExp of expr
  | CDecl of name * expr
  | CRecDecl of name * name * expr
  | CDeclseq of command * command

type env = (name * thunk) list
and value =
  | VInt  of int
  | VBool of bool
  | VFun of name * expr * env
  | VRecFun of name * name * expr * env (* 関数の名前, 引数の名前, 式, 環境 *)
  | VPair of value * value
  | VNil
  | VCons of value * value
and thunk = Thunk of expr * env


let print_name = print_string

let rec print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (_,_,_) -> print_string "<fun>"
  | VRecFun (_,_,_,_) -> print_string "<fun>"
  | VPair (v1, v2) -> 
    (
      print_string "(";
      print_value v1;
      print_string ",";
      print_value v2;
      print_string ")"
    )
  | VNil -> print_string "[]"
  | VCons (v1, v2) ->
    (
      print_value v1;
      print_string " :: ";
      print_value v2
    )

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   https://ocaml.org/api/Format.html
 を活用すること
*)
let rec print_expr e =
  match e with
  | EConstInt i ->
      print_int i
  | EConstBool b ->
      print_string (string_of_bool b)
  | EVar x ->
      print_name x
  | EAdd (e1,e2) ->
      print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ESub (e1,e2) ->
      print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EMul (e1,e2) ->
      print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EDiv (e1,e2) ->
      print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EEq (e1,e2) ->
      print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ELt (e1, e2) ->
      print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EIf (e1,e2,e3) ->
      print_string "EIf (";
      print_expr   e1;
      print_string ",";
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")"
  | ELet (n,e1,e2) ->
      print_string "ELet (";
      print_name n;
      print_string ",";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ELetRec (f,x,e1,e2) ->
      print_string "ELetRec (";
      print_name f;
      print_string ",";
      print_name x;
      print_string ",";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EAnd (e1,e2) ->
      print_string "EAnd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EOr (e1,e2) ->
      print_string "EOr (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EFun (n, e) ->
      print_string "EFun (";
      print_name n;
      print_string ",";
      print_expr e;
      print_string ")"
  | EApp (e1, e2) ->
      print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | EPair (e1, e2) ->
      print_string "EPair (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"
  | ENil ->
      print_string "ENil"
  | ECons (e1, e2) ->
      print_string "ECons (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")"

let rec print_command p =
(* どこで使う？？ *)
  match p with
  | CExp e -> print_expr e
  | CDecl (v, e) ->
    print_name v;
    print_string " =: : : ";
    print_expr e
  | CRecDecl (f, x, e) ->
    print_string "VRec (";
    print_name f;
    print_string ",";
    print_name x;
    print_string ",";
    print_expr e;
    print_string ")"
  | CDeclseq (c, rest) ->
    print_command c;
    print_string " =: : : ";
    print_command rest
