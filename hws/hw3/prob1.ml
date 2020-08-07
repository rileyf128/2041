(*Solution to part 1*)
(*divide_list : ('a -> bool) -> 'a list -> 'a list * 'a list*)
(*Use reduce for this function
*)
let rec reduce f lst u =
  match lst with 
    | [] -> u
    | (h::t) -> f h (reduce f t u)

let divide_list funbool lst = reduce (fun x -> (funbool x)) lst []
