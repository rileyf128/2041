let rec accumulate f lst u =
      match lst with
      | [] -> u
      | (h::t) -> accumulate f t (f h u)


    let append l1 l2 = reduce (fun x -> x) (l1@l2) []
    let reverse l1 = accumulate (fun x-> x) l1 []
    let filter f l1 = reduce f l1 []
