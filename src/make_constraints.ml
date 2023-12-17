open Syntax
open Unifier
exception Unbound
exception TyError

(* 型スキーム型環境を追加する関数 *)
let extend_tyenv (n:name) (tyscm:Unifier.tyscm) (tyenv:tyenv) = (n, tyscm) :: tyenv

(* 変数名に対応する型スキームを型環境から引っ張ってくる *)
let rec lookup_tyenv (n:name) (tyenv:tyenv) =
match tyenv with
| [] -> raise Unbound
| (name, tyscm) :: xs -> if (name = n) then tyscm else lookup_tyenv n xs

(* 型スキームの全称量化されている変数名のリストを受け取って，置換を返す． *)
let rec make_sigma_from_tyvar_lis tyvar_lis = 
match tyvar_lis with
| [] -> []
| n :: ns -> (
    let n_newtyvar = Unifier.new_tyvar () in
    Unifier.compose [(n, Unifier.TyVar n_newtyvar)] (make_sigma_from_tyvar_lis ns)
)

(* 型スキームの∀a_1a_2...t に対して新しい型変数b_1, b_2, ..., を用意してt[a_1:=b_1, ...] で置換する *)
let make_newtyvar_subst tyscm =
let (tyvar_lis, ty) = tyscm in
let sigma = make_sigma_from_tyvar_lis tyvar_lis in
Unifier.ty_subst sigma ty

(* 型の自由変数として，tyvarが入っているか *)
(*
let rec search_tyvar tyvar t =
match t with
| TyInt | TyBool -> false
| TyFun(t1, t2) -> (
    let ret1 = search_tyvar tyvar t1 in
    let ret2 = search_tyvar tyvar t2 in
    ret1 || ret2
)
| TyVar tv -> if tv = tyvar then true else false
| TyPair(t1, t2) -> (
    let ret1 = search_tyvar tyvar t1 in
    let ret2 = search_tyvar tyvar t2 in
    ret1 || ret2
)
*)


(* 型スキームの全称量化の束縛変数の中に，tyvarがふくまれるか．*)
let rec search_tyvar_in_tyvar_lis tyvar tyvar_lis = 
match tyvar_lis with
| [] -> false
| n :: xs -> (
    if n = tyvar then true 
    else search_tyvar_in_tyvar_lis tyvar xs
)

(* 
  単一の置換 & 全称量化の束縛変数のリスト & 型 
  を受け取って，自由変数として存在する型変数を置換する．
*)
let local_subst_tyenv_sub local_subst tyvar_lis ty =
let (tv_target, _) = local_subst in
if (search_tyvar_in_tyvar_lis tv_target tyvar_lis) then ty
else (
    let t_ret = Unifier.ty_subst [local_subst] ty in
    t_ret
)


let rec subst_tyenv_sub subst tyscm =
match subst with
| [] -> tyscm
| (tyvar_target, ty_target) :: subst_s -> (
    let (tyvar_lis, ty) = tyscm in
    subst_tyenv_sub subst_s (tyvar_lis, local_subst_tyenv_sub (tyvar_target, ty_target) tyvar_lis ty)
)

let rec subst_tyenv subst tyenv = 
match tyenv with
| [] -> []
| (tv, ts) :: xs -> (tv, (subst_tyenv_sub subst ts)) :: (subst_tyenv subst xs)

(* 型tの中の型変数を求める *)
let rec get_set_tyvar t = 
match t with
| TyInt | TyBool -> []
| TyFun(t1, t2) -> (
    let ret1 = get_set_tyvar t1 in
    let ret2 = get_set_tyvar t2 in
    ret1 @ ret2
)
| TyVar tv -> [tv]
| TyPair (t1, t2) -> 
    let ret1 = get_set_tyvar t1 in
    let ret2 = get_set_tyvar t2 in
    ret1 @ ret2
| TyList t -> get_set_tyvar t


let rec isincluded_sub tv tvlis =
match tvlis with
| [] -> false
| tv_ :: tvs -> if tv = tv_ then true else isincluded_sub tv tvs

let rec isincluded tv delta =
match delta with
| [] -> false
| (_, (_, t)) :: delta_s -> (
    let set_of_tyvar_in_t = get_set_tyvar t in
    if (isincluded_sub tv set_of_tyvar_in_t) then true 
    else (isincluded tv delta_s)
)

let get_s1_minus_delta s1 delta =
let s1_lis = get_set_tyvar s1 in
let rec get_s1_minus_delta_sub s1_lis delta =
match s1_lis with
| [] -> []
| tv :: tvs -> (
    if isincluded tv delta then get_s1_minus_delta_sub tvs delta
    else tv :: (get_s1_minus_delta_sub tvs delta)
)
in get_s1_minus_delta_sub s1_lis delta



let empty_constraints = []
let extend_constraints (t1:Unifier.ty) (t2:Unifier.ty) constraints = (t1, t2) :: constraints
let union_constraints c1 c2 = c1 @ c2

(*
let lookup n tyenv = 
    try List.assoc n tyenv with Not_found -> raise Unbound
*)

let rec make_constraints (tyenv:tyenv) e =
    (* rerurn (type, constraints) *)
    match e with
    | EConstInt _ ->
        (Unifier.TyInt, empty_constraints)
    | EConstBool _ ->
        (Unifier.TyBool, empty_constraints)
    | EAdd (e1, e2) | ESub (e1,e2) | EMul (e1,e2) | EDiv (e1,e2) ->
        let (t1, c1) = make_constraints tyenv e1 in
        let (t2, c2) = make_constraints tyenv e2 in 
        let c = extend_constraints
            (Unifier.TyFun(Unifier.TyInt, Unifier.TyFun(Unifier.TyInt, Unifier.TyInt)))
            (Unifier.TyFun(t1,             Unifier.TyFun(t2,             Unifier.TyInt)))
            (union_constraints c1 c2) in
        (Unifier.TyInt, c)
    | EAnd (e1, e2) | EOr (e1, e2) ->
        let (t1, c1) = make_constraints tyenv e1 in
        let (t2, c2) = make_constraints tyenv e2 in 
        let c = extend_constraints
            (Unifier.TyFun(Unifier.TyBool, Unifier.TyFun(Unifier.TyBool, Unifier.TyBool)))
            (Unifier.TyFun(t1,              Unifier.TyFun(t2,              Unifier.TyBool)))
            (union_constraints c1 c2) in
        (Unifier.TyBool, c)
    | EEq (e1, e2) | ELt(e1, e2) ->
        let (t1, c1) = make_constraints tyenv e1 in
        let (t2, c2) = make_constraints tyenv e2 in 
        let c = extend_constraints
            (Unifier.TyFun(Unifier.TyInt, Unifier.TyFun(Unifier.TyInt, Unifier.TyBool)))
            (Unifier.TyFun(t1,             Unifier.TyFun(t2,             Unifier.TyBool)))
            (union_constraints c1 c2) in
        (Unifier.TyBool, c)
    | EVar x ->
        let tyscm = lookup_tyenv x tyenv in
        let t = make_newtyvar_subst tyscm in
        (t, empty_constraints)
    | ELet (n, e1, e2) ->
        let (t1, c1) = make_constraints tyenv e1 in
        let sigma = Unifier.unify c1 in
        let s1 = Unifier.ty_subst sigma t1 in
        (* now implementing... *)
        let delta = subst_tyenv sigma tyenv in
        let p = get_s1_minus_delta s1 delta in
        let tyenv' = extend_tyenv n (p, s1) delta in
        let (t2, c2) = make_constraints tyenv' e2 in
        (t2, union_constraints c1 c2)
    | EIf (e1, e2, e3) ->
        let (t1, c1) = make_constraints tyenv e1 in
        let (t2, c2) = make_constraints tyenv e2 in
        let (t3, c3) = make_constraints tyenv e3 in 
        let c = extend_constraints t1 Unifier.TyBool
            (extend_constraints t2 t3
            (union_constraints c1 (union_constraints c2 c3))) in 
        (t2, c)
    | EFun (x, e) ->
        let alpha = Unifier.TyVar (Unifier.new_tyvar ()) in
        let tyenv' = extend_tyenv x ([], alpha) tyenv in
        let (t, c) = make_constraints tyenv' e in
        (Unifier.TyFun(alpha, t), c)
    | EApp (e1, e2) ->
        let (t1, c1) = make_constraints tyenv e1 in
        let (t2, c2) = make_constraints tyenv e2 in 
        let alpha = Unifier.TyVar (Unifier.new_tyvar ()) in
        let c = extend_constraints t1 (Unifier.TyFun(t2, alpha)) (union_constraints c1 c2) in
        (alpha, c)
    | ELetRec (f, x, e1, e2) ->
        let alpha = Unifier.TyVar (Unifier.new_tyvar ()) in
        let beta = Unifier.TyVar (Unifier.new_tyvar ()) in
        let (t1, c1) = make_constraints (extend_tyenv f ([], (Unifier.TyFun(alpha, beta))) (extend_tyenv x ([], alpha) tyenv)) e1 in
        let (t2, c2) = make_constraints (extend_tyenv f ([], (Unifier.TyFun(alpha, beta))) tyenv) e2 in
        (t2, extend_constraints t1 beta (union_constraints c1 c2))
    | EPair (e1, e2) ->
        let (t1, c1) = make_constraints tyenv e1 in 
        let (t2, c2) = make_constraints tyenv e2 in
        (TyPair(t1, t2), union_constraints c1 c2)
    | ENil ->
        let alpha = Unifier.TyVar (Unifier.new_tyvar ()) in
        (TyList(alpha), empty_constraints)
    | ECons (e1, e2) ->
        let (t1, c1) = make_constraints tyenv e1 in
        let (t2, c2) = make_constraints tyenv e2 in 
        (t2, extend_constraints (TyList(t1)) t2 (union_constraints c1 c2))

let rec make_constraints_cmd tyenv cmd = 
    match cmd with
    | CExp e -> 
        let (t, c) = make_constraints tyenv e in
        (t, c, tyenv)
    | CDecl (n, e) ->
        let (t, c) = make_constraints tyenv e in
        let sigma = Unifier.unify c in
        let s = Unifier.ty_subst sigma t in
        let delta = subst_tyenv sigma tyenv in
        let p = get_s1_minus_delta s delta in
        let newtyenv = extend_tyenv n (p, s) delta in
        (t, c, newtyenv)
    | CRecDecl (f, x, e) -> 
        let alpha = Unifier.TyVar (Unifier.new_tyvar ()) in
        let beta = Unifier.TyVar (Unifier.new_tyvar ()) in
        let (t, c) = make_constraints (extend_tyenv f ([], (Unifier.TyFun(alpha, beta))) (extend_tyenv x ([], alpha) tyenv)) e in
        let newtyenv = extend_tyenv f ([], (Unifier.TyFun(alpha, beta))) tyenv in
        let c' = extend_constraints t beta c in
        (t, c', newtyenv)
    | CDeclseq (cdecl, rest) ->
        let (_, _, newtyenv) = make_constraints_cmd tyenv cdecl in
        make_constraints_cmd newtyenv rest