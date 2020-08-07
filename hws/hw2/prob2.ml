(*In the case of a fibonacci the first two numbers of the sequence will always be 1. After that every iteration will be equal to f(n) = f(n-1) + f(n-2). Use ideas from 'fact' and 'reverse as shown in class *)
 let rec fib' n fib1 fib2 i =  
 if i = n then fib1
 else fib' n fib2 (fib1 + fib2) (i + 1)

let fib n = fib' n 1 1 1
 
