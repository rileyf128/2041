(*Need to use option type to deal with situation when no entry exists. Will only need to check after correct name has been found. Use typematching to check if there is None or Some *)

let rec find_salary  = fun dataB (name:string) -> 
  match dataB with
  | [] -> 0.1 
  | h::t -> match h with 
     |(curName:string) , (number:string), (salary:float) -> match curName = name with
        |false -> find_salary t name
        |true -> match Some salary with
             |Some salary -> salary
             |None -> 0.1
          

let rec find_phno = fun dataB (name:string) -> 
   match dataB with
   | [] -> "Empty list"  
   | h::t -> match h with 
     |(curName:string) , (number:string), (salary:float)-> match curName = name with
        |false -> (find_phno t name) 
        |true -> match Some number  with 
          |Some number -> number
          |None -> "Not found"

