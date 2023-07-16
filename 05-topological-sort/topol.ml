(*
	Zadanie:     Topol
	Autor:       Dominik Wawszczak grupa nr 1
	Weryfikator: Jakub Kądziołka grupa nr 1
*)

open PMap;;

exception Cykliczne;;

let topol edges_list =
	let graph = ref empty in
	let visited = ref empty in
	let all_vertices = ref [] in
	
	let add_vertex u =
		if not (mem u !graph) then begin
			graph := add u [] !graph;
			visited := add u false !visited;
			all_vertices := u::(!all_vertices);
		end
	in
	
	List.iter (fun (u, adj) -> begin
		add_vertex u;
		graph := add u (adj @ (find u !graph)) !graph;
		List.iter add_vertex adj;
	end) edges_list;
	
	let post_order = ref [] in
	let where = ref empty in
	let curr_pos = ref 0 in
	let rec dfs u =
		if not (find u !visited) then begin
			visited := add u true !visited;
			List.iter dfs (find u !graph);
			post_order := u::(!post_order);
			where := add u !curr_pos !where;
			curr_pos := !curr_pos + 1;
		end
	in
	List.iter dfs !all_vertices;
	
	List.iter (fun u -> begin
		List.iter (fun v -> begin
			if find u !where <= find v !where then
				raise Cykliczne;
		end) (find u !graph);
	end) !all_vertices;
	
	!post_order;;
