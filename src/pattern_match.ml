open Syntax
(* c.f.

type value =
  | VInt  of int
  | VBool of bool
  | VFun of name * expr * env
  | VRecFun of name * name * expr * env (* 関数の名前, 引数の名前, 式, 環境 *)
  | VPair of value * value
  | VNil
  | VCons of value * value

*)

type pattern = 
    | PInt of int 
    | PBool of bool 
    | PVar of name
    | PPair of pattern * pattern
    | PNil
    | PCons of pattern * pattern

type find_match_result = Success of (name, value) list | Failure

let rec find_match pattern value =
match (pattern, value) with
(* int がマッチするか． *)
| (PInt pi, VInt vi) -> if (pi = vi) then Success [] else Failure
| (PVar x, VInt vi) -> Success [(x, VInt vi)]
| (_, VInt vi) -> Failure
(* boolがマッチするか． *)
| (PBool pb, VBool vb) -> if (pb = vb) then Success [] else Failure
| (PVar x, VBool vb) -> Success [(x, VInt vb)]
| (_, VBool vb) -> Failure
(* fun, recfunがマッチするか． *)
| (PVar x, VFun(n, e, env)) -> Success [(x, VFun(n, e, env))]
| (PVar x, VRecFun(n1, n2, e, env)) -> Success [(x, VRecFun(n1, n2, e, env))]
| (_, VFun(n, e, env)) -> Failure
| (_, VRecFun(n1, n2, e, env)) -> Failure
(* pairがマッチするか． *)
| (PPair(pp1, pp2), VPair(vp1, vp2)) -> 
    let ret1 = find_match pp1 vp1 in
    let ret2 = find_match pp2 vp2 in
    (
        match (ret1, ret2) with
        | (Success r1lis, Success r2lis) -> Success (r1lis @ r2lis)
        | (_, _) -> Failure
    )
| (PVar x, VPair(vp1, vp2)) -> Success [(x, VPair(vp1, vp2))]
| (_, VPair(vp1, vp2)) -> Failure
(* Nilがマッチするか． *)
| (PNil, VNil) -> Success []
| (PVar x, VNil) -> Success [(x, VNil)]
| (_, VNil) -> Failure
(* consがマッチするか．　 *)
| (PCons(pc1, pc2), VCons(vc1, vc2)) ->
    let ret1 = find_match pc1 vc1 in
    let ret2 = find_match pc2 vc2 in
    (
        match (ret1, ret2) with
        | (Success r1lis, Success r2lis) -> Success (r1lis @ r2lis)
        | (_, _) -> Failure
    )
| (PVar x, VCons(vc1, vc2)) -> Success [(x, VCons(vc1, vc2))]
| (_, VCons(vc1, vc2)) -> Failure