
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyList of ty list
  | TyProj of int * ty
  | TyUnit
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmString of string
  | TmConcat of term * term
  | TmFix of term
  | TmTuple of term list
  | TmList of term list
  | TmProj of term * int
  | TmUnit
  | TmHead of term
;;


type command =
    Eval of term
  | Bind of string * term
;;

type binding =
  TyBind of ty
  | TyTmBind of ty * term
;;

type context =
  (string * binding) list
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx x ty =
  (x, TyBind ty) :: ctx
;;

let addbinding ctx x ty tm =
  (x, TyTmBind (ty, tm)) :: ctx
;;

let gettbinding ctx s =
  match List.assoc s ctx with
    TyBind ty -> ty
    |TyTmBind (ty,_) -> ty
;;

let getvbinding ctx s =
  match List.assoc s ctx with
    TyTmBind (_,tm) -> tm
    | _ -> raise Not_found
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"
  | TyTuple tys ->
      "(" ^ String.concat ", " (List.map string_of_ty tys) ^ ")"
  | TyList tys ->
      "[" ^ String.concat "; " (List.map string_of_ty tys) ^ "]"
  | TyProj (i, ty) ->
      string_of_ty ty ^ "." ^ string_of_int i
  | TyUnit ->
      "Unit"
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-String *)
  | TmString s ->
      TyString

    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "arguments of ++ are not strings")
    
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT12 = tyT11 then tyT12
             else raise (Type_error "result of body not compatible with domain")
         | _ -> raise (Type_error "arrow type expected"))
    (* T-Tuple *)
  | TmTuple ts ->
      TyTuple (List.map (typeof ctx) ts)

    (* T-Proj *)
  | TmProj (t, i) ->
      let ty = typeof ctx t in
        (match ty with
          | TyTuple tys ->
              if i <= List.length tys then List.nth tys (i-1)
              else raise (Type_error "index out of bounds in tuple projection")
          | _ -> raise (Type_error "tuple type expected"))
    (* T-List *)
  | TmList ts ->
    let elemTy = match ts with
      [] -> raise (Type_error "empty list")
    | hd :: _ -> typeof ctx hd
    in
    let sameType = List.for_all (fun t -> typeof ctx t = elemTy) ts in
    if sameType then TyList [elemTy]
    else raise (Type_error "elements of list have different types")
    (* T-Unit *)
  | TmUnit ->
      TyUnit
 | TmHead t ->
      (match typeof ctx t with
      | TyList (tyHead :: _) -> tyHead
      | _ -> failwith "Argument of head is not a non-empty list")
  ;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmString s -> "\"" ^ s ^ "\""
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmConcat (t1, t2) ->
      "(" ^ string_of_term t1 ^ " ++ " ^ string_of_term t2 ^ ")"
  | TmFix t ->
      "fix " ^ "(" ^ string_of_term t ^ ")"
  | TmTuple terms ->
      "(" ^ String.concat ", " (List.map string_of_term terms) ^ ")"
  | TmList terms ->
    "[" ^ String.concat "; " (List.map string_of_term terms) ^ "]"
  | TmProj (t, i) ->
      string_of_term t ^ "." ^ string_of_int i
  | TmUnit ->
    "unit"
  | TmHead t ->
      string_of_term t
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmString s ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmFix t ->
      free_vars t
  | TmTuple(ts) -> 
    List.fold_left lunion [] (List.map free_vars ts)
  | TmList ts ->
    List.fold_left lunion [] (List.map free_vars ts)
  | TmProj (t, _) -> 
      free_vars t
  | TmUnit -> []
  | TmHead t ->
      free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmString s -> TmString s
  | TmConcat (t1, t2) -> TmConcat (subst x s t1, subst x s t2)
  | TmFix t ->
      TmFix (subst x s t)
  | TmTuple ts -> 
      TmTuple (List.map (fun t -> subst x s t) ts)
  | TmList ts ->
    TmList (List.map (fun t -> subst x s t) ts)
  | TmProj (t, i) -> 
      TmProj (subst x s t, i)
  | TmUnit -> TmUnit
  | TmHead t ->
    TmHead (subst x s t)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | TmTuple lst -> List.for_all isnumericval lst
  | TmProj (t, _) -> isnumericval t
  | TmList lst -> List.for_all isnumericval lst
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmTuple tmlist -> List.for_all isval tmlist
  | TmList tmlist -> List.for_all isval tmlist
  | TmProj (t, _) -> true
  | t when isnumericval t -> true
  | TmString _ -> true
  | TmUnit -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2) 

    (* E-Concat*)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

  | TmConcat (TmString s1, t2) ->
    let t2' = eval1 ctx t2 in
    TmConcat (TmString s1, t2')
  
  | TmConcat (t1, t2) ->
    let t1' = eval1 ctx t1 in
    TmConcat (t1', t2)

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t12)) ->
      subst x tm t12

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

    (* E-Tuple *)
  | TmTuple ts ->
      let ts' = List.map (eval1 ctx) ts in
      TmTuple ts'

    (* E-List *)
  | TmList tmlist ->
    let tmlist' = List.map (eval1 ctx) tmlist in
    TmList tmlist'

    (* E-Proj *)
  | TmProj (TmTuple tms, n) when n > 0 && n <= List.length tms ->
      List.nth tms (n - 1)

  | TmProj (t1, n) ->
      let t1' = eval1 ctx t1 in
      TmProj (t1', n)

    (*TmVar*)
  | TmVar s -> 
      getvbinding ctx s

    (* E-TmHead *)
  | TmHead (TmList (h :: _)) when isval h ->
    h

  | _ ->
      raise NoRuleApplies 
;;

let apply_ctx ctx tm =
  List.fold_left ( fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

let execute ctx = function
  Eval tm ->
    let tyTm = typeof ctx tm in
    let tm' = eval ctx tm in
    print_endline( "- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm' );
    ctx
| Bind (s, tm) ->
  let tyTm = typeof ctx tm in
  let tm' = eval ctx tm in
  print_endline(s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm' );
  addbinding ctx s tyTm tm'

