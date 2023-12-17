exception Not_found
exception UnifyErr
exception TyError

type tyvar = string

type tyenv = (tyvar * tyscm) list
and tyscm = (tyvar list * ty)
and ty = TyInt | TyBool | TyFun of ty * ty | TyVar of tyvar | TyPair of ty * ty | TyList of ty

type subst = (tyvar * ty) list

let is_eq_tyvar t1 t2 = t1 = t2

type ret_type = Ng | Ok of ty

let r = ref 0

let new_tyvar (dummy:unit) = 
r := !r + 1;
(*print_string ("alpha"^(string_of_int !r));*)
"'a"^(string_of_int !r)

let print_type ty = 
let rec string_of_type ty =
match ty with
| TyInt -> "int"
| TyBool -> "bool"
| TyFun(t1,t2) -> (
    let string_of_t1 = string_of_type t1 in
    let string_of_t2 = string_of_type t2 in
    " ( " ^ string_of_t1 ^ " -> " ^ string_of_t2 ^ " ) "
)
| TyVar n -> n
| TyPair(t1, t2) -> (
    let string_of_t1 = string_of_type t1 in
    let string_of_t2 = string_of_type t2 in
    " ( " ^ string_of_t1 ^ " * " ^ string_of_t2 ^ " ) "
)
| TyList t -> 
    let string_of_t = string_of_type t in
    string_of_t^" list"
in print_string (string_of_type ty)

let rec lookup_type (subst:subst) tyvar =
match subst with
| [] -> Ng
| (tv, t) :: xs -> if (is_eq_tyvar tv tyvar) then Ok t else lookup_type xs tyvar

let rec ty_subst subst ty =
match ty with
| TyInt -> TyInt
| TyBool -> TyBool
| TyFun(t1, t2) -> (
    let t1' = ty_subst subst t1 in 
    let t2' = ty_subst subst t2 in
    TyFun(t1', t2')
)
| TyVar(tv) -> (
    let ret = lookup_type subst tv in
    match ret with
    | Ng -> TyVar(tv)
    | Ok t -> t
)
| TyPair(t1, t2) -> (
    let t1' = ty_subst subst t1 in 
    let t2' = ty_subst subst t2 in 
    TyPair(t1', t2')
)
| TyList t -> TyList (ty_subst subst t)

(* (∀a.a->a)[a := int] = ∀a.a->a に注意 *)
let rec tyenv_subst subst (tyenv:tyenv) =
match tyenv with
| [] -> []
| (n, (tyscm, ty)) :: xs -> (n, (tyscm, ty_subst subst ty)) :: (tyenv_subst subst xs)


let rec subst_with_local_subst local_subst subst =
match subst with
| [] -> []
| (tv, t) :: xs -> (tv, ty_subst local_subst t) :: (subst_with_local_subst local_subst xs)

let compose (s1:subst) (s2:subst) =
(* 
    s1 ○ s2 であることに注意 
    s1のkeyとなっている型変数が，s2のvalueとなっている場合に，
    それをs1の対応するvalueで置き換える．
    その後に，s1を最後にくっつける．
*)
let rec compose_sub s1 s2 = 
match s1 with
| [] -> s2
| (n, t) :: xs -> (
    let local_subst = [(n, t)] in
    let s2' = subst_with_local_subst local_subst s2 in
    compose_sub xs s2'
) in ((compose_sub s1 s2) @ s1 : subst)

let rec subst_all_ty local_subst constraints =
match constraints with
| [] -> []
| (t1, t2) :: xs -> ((ty_subst local_subst t1), (ty_subst local_subst t2)) :: (subst_all_ty local_subst xs)

let rec unify constraints =
match constraints with
| [] -> []
| (t1, t2) :: constraints_rest -> (
    if t1 = t2 then unify constraints_rest
    else (
        match (t1, t2) with
        | (TyFun(s, t), TyFun(s', t')) -> unify ((s, s') :: (t, t') :: constraints_rest)
        | (TyPair(s, t), TyPair(s', t')) -> unify ((s, s') :: (t, t') :: constraints_rest)
        | (TyList(t), TyList(t')) -> unify ((t, t') :: constraints_rest)
        | (TyVar n, t) -> (
            let local_subst = [(n, t)] in
            let new_constraints = subst_all_ty local_subst constraints in
            compose (unify new_constraints) [n, t]
        ) 
        | (t, TyVar n) -> (
            let local_subst = [(n, t)] in
            let new_constraints = subst_all_ty local_subst constraints in
            compose (unify new_constraints) [n, t]
        )
        | _ -> raise TyError
    )
)

