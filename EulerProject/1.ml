
let addEven sum x = if x mod 2 == 0 then sum + x else sum

let fibonacci x =
	let rec fib_acc x acc acc2 = 
		match x with
		| 0 -> acc
		| _ -> fib_acc (x-1) acc2 (acc+acc2)
	in fib_acc x 0 1

let sumFib x =
	let rec aux index sum = 
		if (fibonacci(index) > x)
			then sum
		else
			aux (index + 1) (addEven sum (fibonacci(index)))
	in aux 1 0



