(*Solution to problem 3*)
let rec reduce f lst u =
     match lst with
     | [] -> u
     | (h::t) -> f h (reduce f t u)

let rec forall2 p l1 l2 =
     match (l1,l2) with
     | ([],[]) -> true
     | ([],_) -> false
     | (_,[]) -> false
     | ((h1::t1),(h2::t2)) ->
          (p h1 h2) && (forall2 p t1 t2)

(* Part 1: In the reduce program we can see that it is a recursive function with 3 variables, f, lst, and u. It can be inferred that lst is of type list as it is matched with an empty list or list with elements. if the list is empty then the second argument, u, is returned. Otherwie it appears that f and h (the item in the list being operated on) is returned and the function is called again. f and u are of type 'a, as there is no operation being done on them that implies they are of a specific variable type. ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>
 

Part 2: forall2 is a recurisve function that takes in 3 arguments, p, l1, l2. l1 and l2 appear to be lists, as they are testing for [] which as an attribute of lists. It is checking for the length of each l1 and l2 as match statement continues, if the lengths are different in returns false. The final match case occurs when there are all elements in l1 and l2 at the same position. If they are present it calls p, and the elements of the list and uses and to logically bind the the elements at h1 and h2 with the rest of the list and calls itself again. p appears to be of type bool as it uses and to check the head with the rest of the list. ('a -> 'b -> bool) -> 'a list -> 'b list -> bool = <fun>
*)

