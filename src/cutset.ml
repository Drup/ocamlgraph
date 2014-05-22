module Make (G : Sig.G) = struct

  (* We use the hash and equal functions provided by the user. *)
  module H = Hashtbl.Make (G.V)

  let find_default d htbl x =
    try H.find htbl x
    with Not_found -> false

  let min_cutset gr first_node =
    let n_labels = H.create (G.nb_vertex gr) in
    let l_labels = H.create (G.nb_vertex gr) in

    let already_processed = H.create (G.nb_vertex gr) in
    let is_already_processed x = find_default false already_processed x in

    let on_the_stack = H.create (G.nb_vertex gr) in
    let is_on_the_stack x = find_default false on_the_stack x in

    let cut_set = ref [] in
    let counter = ref 1 in

    let rec step2 top rest_of_stack =
      assert (not (is_already_processed top));
      assert (not (is_on_the_stack top));
      H.add on_the_stack top true;
      H.add n_labels top !counter;
      counter := !counter + 1;
      H.add l_labels top 0;
      H.add already_processed top true;
      step3 (G.succ_e gr top) top rest_of_stack

    and step3 edges top rest_of_stack =
      match edges with
        | edge::other_edges ->
            let v = G.E.dst edge in
            if not (is_already_processed v)
            (* step 4 *)
            then step2 v ((top,edges)::rest_of_stack)
            (* step 5 *)
            else
              begin
                let x =
                  if is_on_the_stack v
                  then H.find n_labels v
                  else H.find l_labels v
                in
                H.add l_labels top
                  (max (H.find l_labels top) x) ;
                step3 other_edges top rest_of_stack
              end

        | [] ->
            begin
              (* step 7 *)
              if H.find l_labels top = H.find n_labels top
              then begin
                cut_set := top::!cut_set ;
                H.add l_labels top 0 ;
              end ;

              (* check added between algorithms C and D *)
              if H.find l_labels top > H.find n_labels top
              then None (* FAILURE (graph not reducible) *)
              else

                (* step 8 *)
                (match rest_of_stack with
                  | [] -> Some !cut_set (* SUCCESS *)
                  | (new_top, new_edges)::new_tail ->
                      begin
                        H.add on_the_stack top false;
                        H.add l_labels new_top
                          (max
                             (H.find l_labels top)
                             (H.find l_labels new_top)) ;
                        step3 new_edges new_top new_tail
                      end)
            end in

    (* step 2 *)
    step2 first_node []

end
