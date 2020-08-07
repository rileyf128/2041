 type ty = BoolTy | IntTy | FunTy of ty * ty

type expr =
       Id of string                     (* for identifiers *)
     | Int of int                       (* for integers *)
     | True                             (* for the boolean value true *)
     | False                            (* for the boolean value false *)
     | Plus of expr * expr              (* for exp1 + exp2 *)
     | Minus of expr * expr             (* for exp1 - exp2 *)
     | Times of expr * expr             (* for exp1 * exp2 *)
     | Div of expr * expr               (* for exp1 / exp2, division being for integers *)
     | Lss of expr * expr               (* for exp1 < exp2 *)
     | Eq of expr * expr                (* for exp1 = exp2, = being equality comparison *)
     | Gtr of expr * expr               (* for exp1 > exp2 *)
     | And of expr * expr               (* for exp1 && exp2 *)
     | Or of expr * expr                (* for exp1 || exp2 *)
     | Not of expr                      (* for not exp *)
     | Cond of expr * expr * expr       (* for if exp1 then exp2 else exp3 *)
     | Let of string * expr * expr      (* for let  = exp1 in exp2 *)
     | Fun of string * ty * expr        (* for fun (x:ty) -> exp *)
     | App of expr * expr               (* for (exp1 exp2) *)

let exp1 = Let ("x",(Int 5),Let ("y",(Int 7),Plus ((Id "x"),(Id "y"))))
let exp2 = Let ("x",True,Cond (Not (Id "x"),True,False))
let exp3 = Let ("x", False, Cond (Not (Id "x"), Let ("x",Int 5,Plus (Id "x", Int 3)), Let ("y",Int 7, Id "y")))

let rec typeof_aux expr lis = 
  match expr with
    Id(str) -> match str with 
      |string -> typeof_aux expr lis 
      |int -> if (typeof_aux expr lis) = int then int else None
      |bool -> if (typeof_aux expr lis) = bool then bool else None

(*|Id(e1)->  if e1 = Id then typeof_aux e1 lis else if e1 = int then Some Int else if e1 = bool then Some Bool else None 
   |Id(e1, e2)->  if e1 = Id then typeof_aux e1 lis else if e2 = Id then typeof_aux e2 lis else if e1 = int && e2 = int then Some Int else if e1 = bool && e2 = bool then Some Bool else None 
   |Id(e1, e2, e3)->  if e1 = Id then typeof_aux e1 lis else if e2 = Id  then typeof_aux e2 lis else if e3 = Id then typeof_aux e3 lis  else if e1 = bool && e2 = int && e3 = int then Some Int else if e1 = bool && e2 = bool && e3 = bool then Some Bool else None *)
