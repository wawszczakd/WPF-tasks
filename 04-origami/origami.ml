(*
	Zadanie:     Origami
	Autor:       Dominik Wawszczak grupa nr 1
	Weryfikator: Jakub Kaszycki grupa nr 1
*)

let eps = 0.000001;;

type point = float * float;;

let square (x: float) : float =
	x *. x;;

(* zwraca odleglosc pomiedzy dwoma punktami *)
let dist_p2p ((x1, y1): point) ((x2, y2): point) : float =
	sqrt (square (x1 -. x2) +. square (y1 -. y2));;

(* prosta opisana rownaniem ax + by + c = 0, przechodzaca przez p1 i p2 *)
(* potrzebujemy p1 i p2 do sprawdzenia czy punkt lezy po odpowiedniej stronie prostej *)
type line = {a: float; b: float; c: float; p1: point; p2: point};;

(* tworzy prosta przechodzaca przez punkty (x1, y1) i (x2, y2) *)
let make_line ((x1, y1): point) ((x2, y2): point) : line =
	{a = y1 -. y2; b = x2 -. x1; c = x1 *. y2 -. x2 *. y1; p1 = (x1, y1); p2 = (x2, y2)};;

(* zwraca odleglosc pomiedzy punktem a prosta *)
let dist_p2l ((x, y): point) (l: line) : float =
	(Float.abs (l.a *. x +. l.b *. y +. l.c)) /. (sqrt (square l.a +. square l.b));;

(* zwraca punkt bedacy odbiciem symetrycznym punktu (x, y) wzgledem prostej l *)
let reflect ((x, y): point) (l: line) : point =
	(* ponizej jest dowod, ze to dziala *)
	(* https://drive.google.com/file/d/0By83v5TWkGjvb2tuekNSUFo3cFE/view?resourcekey=0-vz7t7qtGz_EIvtvW8W4fuw *)
	((x *. ((square l.b) -. (square l.a)) -. 2.0 *. l.a *. (l.b *. y +. l.c)) /. ((square l.a) +. (square l.b)),
	 (y *. ((square l.a) -. (square l.b)) -. 2.0 *. l.b *. (l.a *. x +. l.c)) /. ((square l.a) +. (square l.b)));;

(* zwraca *)
(*  0, jezeli punkt (x, y) lezy na prostej l *)
(*  1, jezeli punkt (x, y) lezy ,,na prawo'' od prostej l (idac od p1 do p2) *)
(*  2, jezeli punkt (x, y) lezy ,,na lewo'' od prostej l (idac od p1 do p2) *)
let side ((x, y): point) (l: line) : int =
	if dist_p2l (x, y) l <= eps then
		(* punkt p lezy na prostej l *)
		0
	else
		(* liczymy iloczyn wektorowy odpowiednich wektorow *)
		let cross_product =
			(x -. (fst l.p1)) *. ((snd l.p2) -. (snd l.p1)) -. ((fst l.p2) -. (fst l.p1)) *. (y -. (snd l.p1))
		in
			if cross_product > 0.0 then
				1
			else
				2;;

type kartka =
	point -> int;;

let prostokat ((x1, y1): point) ((x2, y2): point) : kartka =
	(* 1 jesli punkt (x, y) jest wewnatrz prostokata *)
	(* 0 w przeciwnym wypadku *)
	fun ((x, y): point) ->
		if x1 -. eps <= x && x <= x2 +. eps && y1 -. eps <= y && y <= y2 +. eps then
			1
		else
			0;;

let kolko (center: point) (r: float) : kartka =
	(* 1 jesli punkt (x, y) jest wewnatrz kolka *)
	(* 0 w przeciwnym wypadku *)
	fun ((x, y): point) ->
		if dist_p2p (x, y) center <= r +. eps then
			1
		else
			0;;

let zloz (p1: point) (p2: point) (k: kartka) : kartka =
	let l = make_line p1 p2 in
	fun p ->
		match side p l with
			| 0 -> k p                       (* jesli punkt p jest na prostej l to nic sie nie dzieje *)
			| 1 -> 0                         (* jesli punkt p jest po prawej od prostej l to nie przebija kartki *)
			| 2 -> (k p) + (k (reflect p l)) (* w przeciwnym wypadku tworzymy dwie kopie punktu p *)
			| _ -> failwith "impossible";;

let skladaj (fold_lines: (point * point) list) (k: kartka) : kartka =
	List.fold_left (fun k (p1, p2) -> zloz p1 p2 k) k fold_lines;;
