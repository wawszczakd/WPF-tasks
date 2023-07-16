(*
	Zadanie:     iSet
	Autor:       Dominik Wawszczak grupa nr 1
	Weryfikator: Michał Orżanowski grupa nr 4
*)


(* lewe poddrzewo, wartosc w korzeniu, prawe poddrzewo, wysokosc, rozmiar *)
type t =
	| Node of t * (int * int) * t * int * int
	| Empty;;

let height root =
	match root with
		| Node(_, _, _, h, _) -> h
		| Empty               -> 0;;

let size root =
	match root with
		| Node(_, _, _, _, s) -> s
		| Empty               -> 0;;

let make l (a, b) r =
	let add x y =
		if x + y < 0 then
			max_int
		else
			x + y
	in
		let length (x, y) =
			if y + 1 - x <= 0 then
				max_int
			else
				y + 1 - x
		in
			Node(l, (a, b), r, (max (height l) (height r)) + 1, add (add (size l) (size r)) (length (a, b)));;

let rec bal l (a, b) r =
	let hl = height l in
	let hr = height r in
	if hl > hr + 2 then
		match l with
			| Node(ll, (la, lb), lr, _, _) ->
				if height ll >= height lr then
					make ll (la, lb) (bal lr (a, b) r)
				else (
					match lr with
						| Node(lrl, (lra, lrb), lrr, _, _) ->
							make (make ll (la, lb) lrl) (lra, lrb) (bal lrr (a, b) r)
						| Empty ->
							assert false
				)
			| Empty ->
				assert false
	else if hr > hl + 2 then
		match r with
			| Node(rl, (ra, rb), rr, _, _) ->
				if height rr >= height rl then
					make (bal l (a, b) rl) (ra, rb) rr
				else (
					match rl with
						| Node(rll, (rla, rlb), rlr, _, _) ->
							make (bal l (a, b) rll) (rla, rlb) (make rlr (ra, rb) rr)
						| Empty ->
							assert false
				)
			| Empty ->
				assert false
	else
		make l (a, b) r;;

(* znajduje wierzcholek z wartoscia <= n *)
let rec find f acc n root =
	match root with
		| Node(l, (a, b), r, _, _) ->
			if n < a then
				f (find f acc n l) root
			else if n > b then
				f (find f acc n r) root
			else
				f acc root
		| Empty ->
			f acc root;;

(* zwraca pare (minimalny element, drzewo bez minimalnego elementu) *)
let remove_min root =
	let f (acc1, acc2) root =
		match root with
			| Node(l, (a, b), r, _, _) ->
				if l = Empty then
					((a, b), r)
				else
					(acc1, bal acc2 (a, b) r)
			| Empty ->
				(acc1, acc2)
	in
		find f ((min_int, min_int), Empty) min_int root;;

(* zwraca pare (maksymalny element, drzewo bez maksymalnego elementu) *)
let remove_max root =
	let f (acc1, acc2) root =
		match root with
			| Node(l, (a, b), r, _, _) ->
				if r = Empty then
					((a, b), l)
				else
					(acc1, bal l (a, b) acc2)
			| Empty ->
				(acc1, acc2)
	in
		find f ((max_int, max_int), Empty) max_int root;;

(* laczy dwa drzewa *)
let merge l r =
	match (l, r) with
		| (Empty, Empty) -> Empty
		| (_, Empty)     -> l
		| (Empty, _)     -> r
		| (_, _) ->
			let ((a, b), new_r) = remove_min r in
			bal l (a, b) new_r;;

(* tworzy puste drzewo *)
let empty =
	Empty;;

(* zwraca czy drzewo jest puste *)
let is_empty root =
	match root with
		| Empty -> true
		| _     -> false;;

(* zwraca trojke (drzewo elementow < x, czy root zaweira x, drzewo elementow > x) *)
let split x root =
	let f (lesser, equal, greater) root =
		match root with
			| Node (l, (a, b), r, _, _) ->
				let new_lesser =
					if a < x then
						bal l (a, min b (x - 1)) lesser
					else if	a = x then
						merge l lesser
					else
						lesser
				in
					let new_greater =
						if b > x then
							bal greater (max a (x + 1), b) r
						else if b = x then
							merge r greater
						else
							greater
					in
						if (a <= x) && (x <= b) then
							(new_lesser, true, new_greater)
						else
							(new_lesser, equal, new_greater)
			| Empty ->
				(lesser, equal, greater)
	in
		find f (Empty, false, Empty) x root;;

(* zwraca czy drzewo zawiera x *)
let mem x root =
	let f acc root =
		match root with
			| Node (_, (a, b), _, _, _) ->
				if (a <= x) && (x <= b) then
					true
				else
					acc
			| Empty -> false
	in
		find f false x root;;

(* dodaje przedzial [x, y] *)
let add (x, y) root =
	let (lesser_x, _, greater_x) = split x root in
	let (_, _, greater_y) = split y greater_x in
	let ((x, _), lesser_x) =
		if ((is_empty lesser_x) == false) && (mem (x - 1) lesser_x) then
			remove_max lesser_x
		else
			((x, y), lesser_x)
	in
		let ((_, y), greater_y) =
			if ((is_empty greater_y) == false) && (mem (y + 1) greater_y) then
				remove_min greater_y
			else
				((x, y), greater_y)
		in
			bal lesser_x (x, y) greater_y;;

(* usuwa przedzial [x, y] *)
let remove (x, y) root =
	let (lesser_x, _, greater_x) = split x root in
	let (_, _, greater_y) = split y greater_x in
	merge lesser_x greater_y;;

(* zamienia [x, y] na f([x, y]), dla kazdego wierzcholka drzewa *)
let rec iter f root =
	match root with
		| Node (l, (a, b), r, _, _) ->
			iter f l;
			f (a, b);
			iter f r
		| Empty ->
			();;

(* fold na drzewie *)
let rec fold f root acc =
	match root with
		| Node (l, (a, b), r, _, _) ->
			fold f r (f (a, b) (fold f l acc))
		| Empty ->
			acc;;

(* zwraca liste elementow drzewa *)
let elements root =
	let rec helper acc root =
		match root with
			| Node(l, (a, b), r, _, _) ->
				helper ((a, b)::(helper acc r)) l
			| Empty ->
				acc
	in
		helper [] root;;

(* zwraca liczbe elementow drzewa >= x *)
let below n root =
	let add x y =
		if x + y <= 0 then
			max_int
		else
			x + y
	in
		let length (x, y) =
			if y + 1 - x < 0 then
				max_int
			else
				y + 1 - x
		in
			let f acc root =
				match root with
					| Node (l, (a, b), _, _, _) ->
						if n >= a then
							add acc (add (length (a, (min n b))) (size l))
						else
							acc
					| Empty ->
						acc
			in
				find f 0 n root;;
