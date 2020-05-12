(*General Functions*) 
let rec sumList lst =
	match lst with
		| [] ->0
		| (h::t) ->h + (sumList t)

let rec map f l =
	match l with
		| [] -> []
		| (x::t) -> (f x) :: map f t

let rec accumulate f lst u = (*same thing as foldleft*)
	match lst with
		| [] -> u
		| (h::t) -> accumulate f t (f u h)

let rec reduce f lst u =  (*Same thing as foldright*)
	match lst with
		| [] -> u
		| (h::t) -> f h (reduce f t u)

	(*reduce functions*) 
let sumList r = reduce (fun x y -> x + y) r 0 (*adds a list*)
let length r = reduce (fun x y -> y + 1) r 0 (*finds length of list*) 
let unzip r =reduce (fun (x1,x2) (y1,y2) ->(x1::y1, x2::y2)) r ([],[]) 
let map f l =reduce (fun x y -> (f x) :: y) l []

let rec filter p l = (*filtering a list based on a property*) 
	match l with
		| [] -> []
		| (h::t) -> if (p h) then h :: (filter p t) else (filter p t)

let rec forall p l = (*checking if every item satisfies a property*) 
	match l with
		| [] -> true
		| (h::t) -> (p h) && (forall p t)

let rec cont_sumlist lst c = (*sumlist using continuation*) 
	match lst with
		| [] -> (c 0)
		| (h::t) -> cont_sumlist t (fun x -> c (h + x))

(*let rec whilestat exp stat = (*while loop*) 
	fun s ->ifstat exp(seq stat (whilestat exp stat))
					(fun x -> x) s *)

let rec fib n =
	if (n = 1) then 1
	else if (n = 2) then 1
	else fib (n-1) + fib (n-2)

let rec append l1 l2 =
	match l1 with
		| [] -> l2
		| (h::t) -> h :: (append t l2)

let rec reverse l =
	match l with
		| [] -> []
		| (h::t) -> append (reverse t) [h]

(*Trees*)

let rec insert t i =
	match t with
		| Empty -> Node (i,Empty,Empty)
		| Node (d,l,r) ->if (i < d) then Node (d,insert l i,r) else Node (d,l,insert r i)

let rec sumTree t =
	match t with
		| Empty -> 0
		| Node (i,l,r) -> i + sumTree l + sumTree r

let rec minTree = 
	function 
		| Empty -> None        
		| Node (i,l,r) ->match (minTree l) with    
				| None -> (Some i)
				| (Some x) as m -> m
let rec maxTree =
	function  
        | Empty -> None
		| Node (i,l,r) -> match (maxTree r) with         
			| None -> (Some i)      
			| (Some x) as m -> m

let rec find t i =
	match t with
		| Empty -> false
		| Node (d,l,r) ->if (i = d) then true else if (i < d) then find l i else find r i


(*Lab 3*)

let rec sumUp n = n + sumUp(n-1)

let flip_pair p = 
   match p with
     |(h1,h2) -> (h2,h1)

let rec flipList l = 
   match l with
     |[] -> []
     |(h1,h2)::t -> (h2,h1)::flipList t

let rec sum_diffs l = 
   match l with 
     [] -> 0
    |(h::[]) -> 0
    |(h1::h2::t) -> h1 - h2 + sum_diffs t 

(*Lab 4*)
let rec sumList l = 
let rec sumList' l c = 
    match l with
      [] -> (c + 0)
     |(h::t) -> sumList' t (c + h) 
in sumList' l 0

(*Lab 05*) 


type 'a btree =
   Empty
   |Node of 'a * 'a btree * 'a btree


let rec maxTree t = 
   match t with
    |Empty -> None
    |Node(i,l,r) -> if r = Empty then Some i else maxTree r 

let rec minTree t = 
   match t with
     |Empty -> None
     |Node(i,l,r) -> if l = Empty then Some i else minTree l
 
 

(*Lab 07*) 

let rec map2 f l1 l2 = 
  match l1 with
    |[] -> []
    |(h1::t1) -> match l2 with
      |[] -> []
      | (h2::t2) -> (f h1)::(f h2)::(map2 f t1 t2)

let rec accumulate f lst u = 
   match lst with 
     [] -> u
    |(h::t) -> accumulate f t (f h u) 

let rec reduce f lst u = 
   match lst with 
     [] -> u
     |(h::t) -> f h (reduce f t u) 

(*Final Practice*)

(*1*)

	(*let f b a = a + b in let g = f 8 in g 8*)
    (*Invalid declaration*) 
	let w = (fun x -> let y = (fun z -> x ^ z)in let x = 17 in (y "to",x)) "go"
	let x = let g x y = x y in let z = g (fun y -> y * 2) in (z 3)	
	(*let rec reverse l =match l with| [] -> []| (h::t) -> (reverse t) :: h (invalid)*)
	(*let z = let y = 7 in y + 10 in z * y   (Invalid declaration)*)

(*2*)

	let rec myfold f lst u =
		match lst with
			| [] -> u
			| (h::t) -> myfold f t (f (u,h))
(*myfold : ('a * 'b -> 'a) -> 'b list -> 'a -> 'a = <fun>*)
	
	let rec append l =
		match l with
			| [] -> (fun l2 -> l2)
			| (h::t) ->let tail_appender = append t 
	in(fun l -> h :: tail_appender l)
(*append : 'a list -> 'a list -> 'a list = <f*)

	let rec zip lpair =
		match lpair with
			| (([],_) | (_,[])) -> []
			| ((h1::t1),(h2::t2)) -> (h1,h2) :: zip (t1,t2)
(*zip : 'a list * 'b list -> ('a * 'b) list = <fun>*)

(*3*)

let rec sumTree_cont t c = 
	match t with
		|Empty -> c + 0
		|Node(i,l,r) -> (sumTree_cont l i) + (sumTree_cont r i)

let rec revapp l1 l2 c = 
	match l1 with 
		|[] -> c l2 
		|(h::t) -> revapp t (h::l2) c

let rec map f l c  = 
	match l with
		|[] -> [] 
		|(h::t) -> c::(map f t (f h))

(*4*) 

	(*1. T(n) = n 
	2.  BC: when [] -> end, BC holds. IH: (h::t) -> iterate through each element therefore T(n) = 			n. T(n + 1) = T(n) + c -> T(n+1) = T(n+1).

	3.T(n) = T(append) + T(n) -> T(n)
	4. BC: [] -> end, BC holds. IH: T(n)  = n + T(append) -> T(n+1) = n + T(append) + c1 + c2 -> 
		T(n+1) = 2n + c1 + c2 -> T(n+1) = n + 1
	
	5. The time a function takes to run is minimal so it depends more on the number of 				calculations. 
	6. Each function operates on each member of the list so an increase in length leads to an increase in time. *)

(*5*)

(*??????*)

(*6*)

let rec unzip l = 
	match l with
		|_-> [],[]
		|(h1,h2)::t -> h1::unzip t 










