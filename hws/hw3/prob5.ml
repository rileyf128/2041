(*solution to part 5*)
type expr =
       Id of string                   (* for identifiers *)
     | Int of int                     (* for integers *)
     | True                           (* for the boolean value true *)
     | False                          (* for the boolean value false *)
     | Plus of (expr * expr)          (* for exp1 + exp2 *)
     | Minus of (expr * expr)         (* for exp1 - exp2 *)
     | Times of (expr * expr)         (* for exp1 * exp2 *)
     | Div of (expr * expr)           (* for exp1 / exp2, division being for integers *)
     | Lss of (expr * expr)           (* for exp1 < exp2 *)
     | Eq of (expr * expr)            (* for exp1 = exp2, = being equality comparison *)
     | Gtr of (expr * expr)           (* for exp1 > exp2 *)
     | And of (expr * expr)           (* for exp1 && exp2 *)
     | Or of (expr * expr)            (* for exp1 || exp2 *)
     | Not of expr                    (* for not exp *)
     | Cond of (expr * expr * expr)   (* for if exp1 then exp2 else exp3 *)
     | Let of (string * expr * expr)  (* for let  = exp1 in exp2 *)
     | Fun of (string * expr)         (* for fun (x:ty) -> exp *)
     | App of (expr * expr)           (* for (exp1 exp2) *)

let rec subst expr str = (*Not done*)
  match expr with 
   |Id(e1) -> if e1 = "x" || e2 = "y" then Id(e1) = Id(str) else subst e1 str
   |_ 

(*let rec freeIn expr str =  
   match str with 
    |expr -> 
    |

let rec subst expr str =
  match str with 
    |freeIn expr -> 


let namecounter = ref 0
let newname () =
     ( namecounter := !namecounter + 1; "var" ^ string_of_int !namecounter) 
*)
