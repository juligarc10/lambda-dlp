
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
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
;;

type command =
    Eval of term
  | Bind of string * term
;;

type binding =
  TyBind of ty
  | TmBind of ty * term
  | TyTmBind of ty * term
;;

type context =
  (string * binding) list
;;

val emptyctx : context;;
val getbinding : context -> string -> ty;;

val addtbinding : binding list -> string -> ty -> binding list
val addbinding : binding list -> string -> binding -> binding list
val gettbinding : binding list -> string -> ty
val getvbinding : binding list -> string -> binding

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> term;;

val execute : context -> command -> context;;

