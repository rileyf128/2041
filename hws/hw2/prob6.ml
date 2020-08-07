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

let rec eval expr lis = 
let eval_aux expr =
  match expr with 
   |Plus(e1, e2)->(e1 + e2) 
   |Minus(e1, e2)->(e1 - e2) 
   |Times(e1, e2)-> (e1 * e2) 
   |Div(e1, e2)-> (e1 / e2) 
   |Lss(e1, e2)-> if e1 < e2 then True else False 
   |Eq(e1, e2)-> if e1 = e2 then True else False 
   |Gtr(e1, e2)-> if e1 > e2 then True else False
   |And(e1, e2)-> e1 && e2
   |Or(e1, e2)-> e1 || e2
   |Cond(e1, e2, e3)-> if e1 = True then e2 else e3
   |Let(str, e1, e2)-> let str = e1 in e2
 in eval
