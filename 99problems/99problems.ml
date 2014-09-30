let rec last = function
	| [] -> None
	| [x] -> Some x
	| _::t -> last t;;

let rec last_two = function
	| [] -> None
	| [_] -> None
	| [a;b] -> Some (a,b)
	| _::t -> last_two t;;

let rec at k = function
	| [] -> None
	| h::t -> if k = 1 then Some h else at (k-1) t;;

let length l = 
	let rec internal acc = function
		| [] -> acc
		| h::tail -> internal (1+acc) tail
	in internal 0 l;;

let length_fold l =
	List.fold_left (+) 0 l;;