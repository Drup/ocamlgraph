module Make (G : Sig.G) = struct

  let min_cutset gr first_node =
    let n_labels : (G.V.t,_) Hashtbl.t = Hashtbl.create (G.nb_vertex gr) in
    let l_labels = Hashtbl.create (G.nb_vertex gr) in
    let already_processed = Hashtbl.create (G.nb_vertex gr) in
    let on_the_stack = Hashtbl.create (G.nb_vertex gr) in
    let cut_set = ref [] in
    let counter = ref 1 in

    let rec step2 top rest_of_stack =
      assert (not (Hashtbl.find already_processed top));
      assert (not (Hashtbl.find on_the_stack top));
      Hashtbl.add on_the_stack top true;
      Hashtbl.add n_labels top !counter;
      counter := !counter + 1;
      Hashtbl.add l_labels top 0;
      Hashtbl.add already_processed top true;
      step3 (G.succ_e gr top) top rest_of_stack

    and step3 edges top rest_of_stack =
      match edges with
        | edge::other_edges ->
            let v = G.E.dst edge in
            if not (Hashtbl.find already_processed v)
            (* step 4 *)
            then step2 v ((top,edges)::rest_of_stack)
            (* step 5 *)
            else
              begin
                let x =
                  if Hashtbl.find on_the_stack v
                  then Hashtbl.find n_labels v
                  else Hashtbl.find l_labels v
                in
                Hashtbl.add l_labels top
                  (max (Hashtbl.find l_labels top) x) ;
                step3 other_edges top rest_of_stack
              end

        | [] ->
            begin
              (* step 7 *)
              if Hashtbl.find l_labels top = Hashtbl.find n_labels top
              then begin
                cut_set := top::!cut_set ;
                Hashtbl.add l_labels top 0 ;
              end ;

              (* check added between algorithms C and D *)
              if Hashtbl.find l_labels top > Hashtbl.find n_labels top
              then None (* FAILURE (graph not reducible) *)
              else

                (* step 8 *)
                (match rest_of_stack with
                  | [] -> Some !cut_set (* SUCCESS *)
                  | (new_top, new_edges)::new_tail ->
                      begin
                        Hashtbl.add on_the_stack top false;
                        Hashtbl.add l_labels new_top
                          (max
                             (Hashtbl.find l_labels top)
                             (Hashtbl.find l_labels new_top)) ;
                        step3 new_edges new_top new_tail
                      end)
            end in

    (* step 2 *)
    step2 first_node []

end
