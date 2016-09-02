let add_clauses clauses =
  List.iter (fun c ->
      let lit_list = List.map (fun (i,b) ->
          let l = Types.Lit.lit i b in
          while (abs l) - 1 > Solver.nVars () do Solver.newVar () done;
          l
        ) c in

      let lit_vec = Vec.Vec.fromList lit_list (List.length lit_list) in

      if not (Solver.addClause lit_vec) then
        Printf.eprintf "Error adding clauses";
    ) clauses


let main =
  Solver.set_debug true;
  (* Set time counter *)
  let start_time = Unix.gettimeofday () in

  let file = Sys.argv.(1) in
  let clauses, nbvars, nbcls = Parser.parse file in

  (*  Set option input *)

  add_clauses clauses;
  Printf.eprintf "%d Clauses added, %d Vars added\n" nbcls nbvars;

  let parse_time = Unix.gettimeofday () in
  Printf.eprintf "Parse time %f\n" (parse_time -. start_time);

  if Solver.simplify () then
    (* print to file argc > 3 *)
    Printf.eprintf "Solved by unit propagation\n";

  let ret = Solver.solve () in

  let end_time = Unix.gettimeofday () in
  Printf.eprintf "Solve time %f\n" (end_time -. parse_time);

  Printf.eprintf "%s\n%!" (if ret then "SATISFIABLE" else "UNSATISFIABLE");

  Solver.printStats (end_time -. start_time);


  (* *)
