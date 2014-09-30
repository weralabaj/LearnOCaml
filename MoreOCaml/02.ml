type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec lseq n =
  Cons (n, fun () -> lseq (n + 1))

 let rec lconst n =
 	Cons(n, fun () -> lconst n)

 let rec lazy3s n =
 	Cons(3*n, fun() -> lazy3s(n+1))

let lazyhd (Cons (n, _)) = n
let lazytl (Cons (_, t)) = t()

 let rec take n (h::t) =
 	match n with 
 		0 -> []
 		| _ -> h :: take (n-1) t

 let rec lazytake n (Cons(h, t)) =
 	match n with 
 		0 -> []
 		| _ -> h :: lazytake (n-1) (t())

 let rec lazydrop n (Cons(h,t) as list) =
 	match n with
 		0 -> list
 		| _ -> lazydrop (n-1) (t())

 let rec lazymap f (Cons (h, t)) =
 	Cons (f h, fun () -> lazymap f (t()))

 let rec lazyfilter f (Cons(h, t)) =
 	if f h then Cons(h, fun () -> lazyfilter f (t()))
 	else lazyfilter f (t())

 let cubes =
 	lazyfilter 
 	(fun x -> x mod 5 = 0)
 	(lazymap (fun x -> x * x) (lseq 1))

let rec interleave (Cons(h, t)) l =
	Cons(h, fun () -> interleave l (t()))

let interleaved =
	interleave (lconst 0) (lconst 1)

let rec allfrom l =
	Cons(l, fun () -> interleave (allfrom (0::l)) (allfrom (1::l)))

let allones = allfrom []


(* Questions *)
(* 1 *)
let rec power2 x =
	match x with
	0 -> 1
	| _ -> 2 * power2 (x-1)

let lazy2s = 
	let rec aux n =
		Cons (power2 n, fun () -> aux (n+1) )
	in aux 0

(* 2 *)
let rec lazyAt n (Cons (h, t)) = 
	match n with
	0 -> h
	| _ -> lazyAt (n-1) (t())

(* 3 *)
(* TO DO *)

(* 4 *)
let rec fib n =
	match n with
	0 -> 0
	| 1 -> 1
	| _ -> fib (n-1) + fib (n - 2)

let rec lazyFib n =
	Cons (fib n, fun () -> lazyFib (n+1))