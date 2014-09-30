let listMultiplies multiply max =
	let rec aux index = 
		let el = index * multiply 
		in 
			if el < max then el::(aux (index + 1)) 
			else []
	in aux 1

let setify l = List.fold_left (fun a e -> if List.mem e a then a else e :: a) [] l

let sumMultiplies max =
	let elements = ((listMultiplies 3 max) @ (listMultiplies 5 max))
	in
		List.fold_left (+) 0 (setify elements)