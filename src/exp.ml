(*
 * File given by "Stephan Monnier" for this Work
 * Define the FJS language Grammar
 *)

open Error;;

(* exp.ml  --  Représentation à la fin du frontend.  *)

(* Représentation des identificateurs.  *)
type id = location * string

(* Opérations primitives.  *)
type oper = Add | Sub | Mul | Div | Neg | Leq | Lt | Eq

(* Expressions du langage.  *)
type exp =
    | Boolean of bool
    | Var of id
    | Num of int
    | Str of string
    | Function of id list * exp
    | Call of location * exp * exp list
    | Select of exp * id
    | If of location * exp * exp * exp
    | Let of (id * exp) list * exp
    | PrimOp of location * oper * exp list

(***************************************************************************)
(****                   Interpréteur simpl(ist)e                        ****)
(***************************************************************************)
type value =
  | Vnum of int
  | Vbool of bool
  | Vstring of string
  | Vfun of (location -> value list -> value)

(* Tables associatives persistentes, indexée par des chaînes de caractères.  *)
module SMap
  = Map.Make (struct type t = string let compare = String.compare end)

let env_init = SMap.empty

let rec env_extend l env xs vs =
  match (xs, vs) with
  | ([], []) -> env
  | ((_, x)::xs, v::vs) -> env_extend l (SMap.add x (ref v) env) xs vs
  | _ -> raise (Error (l, "wrong number of arguments"))

let rec eval (env : value ref SMap.t) (e : exp) : value = match e with
  | Boolean value -> if value then Vbool true else Vbool false
  | Var ((_, x)) -> !(SMap.find x env)
  | Num n -> Vnum n
  | Str s -> Vstring s
  | Function (fargs, body)
    -> Vfun (fun l aargs -> eval (env_extend l env fargs aargs) body)
  | Call (l, f, args)
    -> (match eval env f with
        | Vfun f -> f l (List.map (eval env) args)
        | _ -> raise (Error (l, "not a function")))
  | Select (e, label)
    -> (match (eval env e, label) with
        | (Vstring s, (_, "length")) -> Vnum (String.length s)
        | (_, (l, _)) -> raise (Error (l, "unknown field")))
  | If (l, e1, e2, e3)
    -> (match eval env e1 with
        | Vbool true -> eval env e2
        | Vbool false -> eval env e3
        | _ -> raise (Error (l, "type mismatch")))
  | Let (decls, e)
    -> let vuninitialized
         = fun l -> Vfun (fun _ -> raise (Error (l, "uninitialized"))) in
       let nenv = List.fold_left
                    (fun env ((l, x), _)
                     -> SMap.add x (ref (vuninitialized l)) env)
                    env decls in
       List.iter (fun ((_, x), e) -> let vr = SMap.find x nenv in
                                     vr := eval nenv e)
                 decls;
       eval nenv e
  | PrimOp (l, o, args)
    -> let args = List.map (eval env) args in
       match (o, args) with
       | (Add, [Vnum n1; Vnum n2]) -> Vnum (n1 + n2)
       | (Sub, [Vnum n1; Vnum n2]) -> Vnum (n1 - n2)
       | (Mul, [Vnum n1; Vnum n2]) -> Vnum (n1 * n2)
       | (Div, [Vnum n1; Vnum n2]) -> Vnum (n1 / n2)
       | (Neg, [Vnum n]) -> Vnum (- n)
       | (Leq, [Vnum n1; Vnum n2]) -> Vbool (n1 <= n2)
       | (Lt, [Vnum n1; Vnum n2]) -> Vbool (n1 < n2)
       | (Eq, [v1; v2]) -> Vbool (v1 = v2)
       | (Neg, [_]) -> raise (Error (l, "type mismatch"))
       | (Neg, _) -> raise (Error (l, "wrong number of arguments"))
       | (_, [_; _]) -> raise (Error (l, "type mismatch"))
       | (_, _) -> raise (Error (l, "wrong number of arguments"))

let vprint v = match v with
  | Vnum n -> string_of_int n
  | Vstring s -> "\"" ^ s ^ "\""
  | Vbool true -> "true"
  | Vbool false -> "false"
  | Vfun _ -> "#<function>"

(* exp.ml ends here.  *)
