(*
	Zadanie:     Przelewanka
	Autor:       Dominik Wawszczak grupa nr 1
	Weryfikator: Patryk Malcher grupa nr 1
*)

let przelewanka a =
	(* pozbycie sie szklanek o zerowej pojemnosci *)
	let a = Array.of_list (List.filter (fun (x, _) -> x <> 0) (Array.to_list a)) in
	
	(* liczba szklanek *)
	let n = Array.length a in
	
	(* pojemnosci szklanek *)
	let volume = Array.init n (fun i -> fst a.(i)) in
	(* poczatkowy stan *)
	let start = Array.make n 0 in
	(* koncowy stan *)
	let goal = Array.init n (fun i -> snd a.(i)) in
	
	(* funkcja zwracajaca najwiekszy wspolny dzielnik dwoch liczb *)
	let rec gcd a b =
		if b = 0 then
			a
		else
			gcd b (a mod b)
	in
	
	let gcd_of_volumes = ref 0 in
	for i = 0 to n - 1 do
		gcd_of_volumes := gcd !gcd_of_volumes volume.(i)
	done;
	
	let ok = ref false in
	(* sprawdzenie czy przynajmniej jedna szklanka na koniec jest pusta lub pelna *)
	for i = 0 to n - 1 do
		if goal.(i) = 0 || goal.(i) = volume.(i) then
			ok := true
	done;
	(* sprawdzenie czy oczekiwana ilosc wody w kazdej szklance jest podzielna przez najwiekszy wspolny dzielnik wszystkich objetosci *)
	for i = 0 to n - 1 do
		if goal.(i) mod !gcd_of_volumes <> 0 then
			ok := false
	done;
	
	(* hashmapa odleglosci, z ktorej bedziemy korzystac w bfs-ie *)
	let dist = Hashtbl.create 1000000 in
	Hashtbl.add dist start 0;
	
	let q = Queue.create () in
	Queue.push start q;
	
	let insert state d =
		if not (Hashtbl.mem dist state) then begin
			Hashtbl.add dist state d;
			Queue.push state q
		end
	in
	
	if !ok then begin
		while not (Queue.is_empty q) && not (Hashtbl.mem dist goal) do
			let state = Queue.pop q in
			let d = Hashtbl.find dist state in
			
			(* dolanie wody do pelna w szklance i *)
			for i = 0 to n - 1 do
				let new_state = Array.copy state in
				new_state.(i) <- volume.(i);
				insert new_state (d + 1)
			done;
			
			(* wylanie wody ze szklanki i *)
			for i = 0 to n - 1 do
				let new_state = Array.copy state in
				new_state.(i) <- 0;
				insert new_state (d + 1)
			done;
			
			(* przelanie wody ze szklanki i do szklanki j *)
			for i = 0 to n - 1 do
				for j = 0 to n - 1 do
					if i <> j then begin
						let new_state = Array.copy state in
						let amount = min state.(i) (volume.(j) - state.(j)) in
						new_state.(i) <- state.(i) - amount;
						new_state.(j) <- state.(j) + amount;
						insert new_state (d + 1)
					end
				done
			done
		done
	end;
	
	if Hashtbl.mem dist goal then
		Hashtbl.find dist goal
	else
		-1;;
