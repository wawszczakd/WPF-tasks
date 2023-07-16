(*
	Zadanie:     Leftist
	Autor:       Dominik Wawszczak grupa nr 1
	Weryfikator: Kajetan Lewandowski grupa nr 1
*)

(* (lewe poddrzewo, wartosc w korzeniu, prawe poddrzewo, wysokosc) *)
type 'a queue = 
	| Node of ('a queue) * 'a * ('a queue) * int
	| Null;;

let empty : ('a queue) =
	Null;;

let rec join (d1: 'a queue) (d2: 'a queue) : ('a queue) =
	match (d1, d2) with
		| (Null, Null) -> Null
		| (Null, _)    -> d2
		| (_, Null)    -> d1
		| (Node(left1, root1, right1, height1), Node(left2, root2, right2, height2)) ->
			if root1 > root2 then
				join d2 d1
			else
				let d3 = join right1 d2 in
				match (left1, d3) with
					| (Null, Null) -> Node(Null, root1, Null, 1)
					| (Null, _)    -> Node(d3, root1, Null, 1)
					| (_, Null)    -> Node(left1, root1, Null, 1)
					| (Node(_, _, _, h1), Node(_, _, _, h2)) ->
						if h1 > h2 then
							Node(left1, root1, d3, h2 + 1)
						else
							Node(d3, root1, left1, h1 + 1);;

let add (value: 'a) (d: 'a queue) : ('a queue) =
	join (Node(Null, value, Null, 1)) d;;

exception Empty;;

let delete_min (d: 'a queue) : ('a * 'a queue) =
	match d with
		| Null                            -> raise Empty
		| Node(left, root, right, height) -> (root, join left right);;

let is_empty (d: 'a queue) : (bool) =
	match d with
		| Null -> true
		| _    -> false;;
