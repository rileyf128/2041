(*Solution to problem 2*)
type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree

let rec treemap btree func =
   match btree with
    |Empty -> Empty
    |Node(i, l, r) ->  Node((func (i)), (treemap l func), (treemap r func)) 
