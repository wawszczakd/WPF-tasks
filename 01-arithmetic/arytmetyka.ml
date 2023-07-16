(*
	Zadanie:     Arytmetyka
	Autor:       Dominik Wawszczak grupa nr 1
	Weryfikator: Maciej Mioduszewski grupa nr 5
*)

type wartosc = {left: float; right: float};;

let rec my_min l =
	match l with
		| []   -> nan
		| h::t ->
			let answer = my_min t in
			if classify_float h = FP_nan || answer < h then
				answer
			else
				h;;

let rec my_max l =
	match l with
		| []   -> nan
		| h::t ->
			let answer = my_max t in
			if classify_float h = FP_nan || answer > h then
				answer
			else
				h;;

let abs x =
	if x >= 0.0 then
		x
	else
		-.x;;

(* jesli x <= y, to (x, y) odpowiada przedzialowi <x, y> *)
(* jesli x > y,  to (x, y) odpowiada sumie przedzialow (neg_infinity, y> i <x, infinity) *)
let wartosc_od_do x y =
	assert(x <> infinity     || y <> infinity);
	assert(x <> neg_infinity || y <> neg_infinity);
	if classify_float x = FP_nan then
		assert(classify_float y = FP_nan);
	if classify_float y = FP_nan then
		assert(classify_float x = FP_nan);
	
	let new_x =
		if x = (-0.0) then 0.0
		else x
	in
	let new_y =
		if y = (-0.0) then (0.0)
		else y
	in
	{left = new_x; right = new_y};;

let wartosc_dokladnosc x p =
	wartosc_od_do (x -. (abs x) *. p /. 100.0) (x +. (abs x) *. p /. 100.0);;

let wartosc_dokladna x =
	wartosc_od_do x x;;

let in_wartosc x y =
	if classify_float x.left = FP_nan then
		false
	else if x.left <= x.right then
		x.left <= y && y <= x.right
	else
		y <= x.right || x.left <= y;;

let min_wartosc x =
	if classify_float x.left = FP_nan then
		nan
	else if x.left <= x.right then
		x.left
	else
		neg_infinity;;

let max_wartosc x =
	if classify_float x.left = FP_nan then
		nan
	else if x.left <= x.right then
		x.right
	else
		infinity;;

let sr_wartosc x =
	if x.left <= x.right then
		(x.left +. x.right) /. 2.0
	else
		nan;;

let plus a b =
	if classify_float a.left = FP_nan || classify_float b.left = FP_nan then
		wartosc_od_do nan nan
	else if a.left <= a.right && b.left <= b.right then
		wartosc_od_do (a.left +. b.left) (a.right +. b.right)
	else if (a.left <= a.right && b.left > b.right) || (a.left > a.right && b.left <= b.right) then
		if a.right +. b.right >= a.left +. b.left then
			wartosc_od_do neg_infinity infinity
		else
			wartosc_od_do (a.left +. b.left) (a.right +. b.right)
	else
		wartosc_od_do neg_infinity infinity;;

let add_inv a =
	wartosc_od_do (-.a.right) (-.a.left);;

let minus a b =
	plus a (add_inv b);;

let rec razy a b =
	if classify_float a.left = FP_nan || classify_float b.left = FP_nan then
		wartosc_od_do nan nan
	else if (a.left = 0.0 && a.right = 0.0) || (b.left = 0.0 && b.right = 0.0) then
		wartosc_od_do 0.0 0.0
	else if a.left <= a.right && b.left <= b.right then
		let l =
			[a.left *. b.left; a.left *. b.right; a.right *. b.left; a.right *. b.right]
		in
			wartosc_od_do (my_min l) (my_max l)
	else if a.left <= a.right && b.left > b.right then
		if a.left > 0.0 then
			let left = max (a.left *. b.right) (a.right *. b.right) in
			let right = min (a.left *. b.left) (a.right *. b.left) in
			if left >= right then
				wartosc_od_do neg_infinity infinity
			else
				wartosc_od_do right left
		else if a.right < 0.0 then
			let left = max (a.left *. b.left) (a.right *. b.left) in
			let right = min (a.left *. b.right) (a.right *. b.right) in
			if left >= right then
				wartosc_od_do neg_infinity infinity
			else
				wartosc_od_do right left
		else
			wartosc_od_do neg_infinity infinity
	else if a.left > a.right && b.left <= b.right then
		razy b a
	else
		if in_wartosc a 0.0 || in_wartosc b 0.0 then
			wartosc_od_do neg_infinity infinity
		else
			wartosc_od_do (min (a.left *. b.left) (a.right *. b.right)) (max (a.left *. b.right) (a.right *. b.left))

let mul_inv a =
	if a.left = 0.0 && a.right = 0.0 then
		wartosc_od_do nan nan
	else if a.left = neg_infinity && a.right = infinity then
		wartosc_od_do neg_infinity infinity
	else if a.left < 0.0 && a.right = 0.0 then
		wartosc_od_do neg_infinity (1.0 /. a.left)
	else
		wartosc_od_do (1.0 /. a.right) (1.0 /. a.left);;

let podzielic a b =
	razy a (mul_inv b);;
