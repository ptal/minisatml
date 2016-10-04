let add_clauses clauses =
  List.iter (fun c ->
      let lit_list = List.map (fun (i,b) ->
          let l = (Types.Lit.lit (i-1) b) in
          while i - 1 >= Solver.nVars () do Solver.newVar () done;
          l
        ) c in

      let lit_vec = Vec.Vec.fromList lit_list (List.length lit_list) in

      if not (Solver.addClause lit_vec) then
        Printf.eprintf "Erroe adding clause \n%!"
    ) clauses


let main =
  let oc = open_out "./trace_decision_satml" in
  
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _signum -> Printf.eprintf "\nTERMINATED\n%!";Solver.printStats 0.;close_out oc;exit 2));

  Solver.set_debug  false;
  (* Set time counter *)
  let start_time = Unix.gettimeofday () in

  let file = Sys.argv.(1) in

  Printf.eprintf
    "============================[ Problem Statistics ]=============================\n%!";
  Printf.eprintf
    "|                                                                             |\n%!";

  let clauses, nbvars, nbcls = Parser.parse file in

  Printf.eprintf
    "|  Number of variables:  %-12d                                         |\n%!"
    nbvars;
  Printf.eprintf
    "|  Number of clauses:    %-12d                                         |\n%!"
    nbcls;

  (*  Set option input *)
  add_clauses clauses;

  (* let parse_time = Unix.gettimeofday () in *)

  if not (Solver.simplify ()) then
    (* print to file argc > 3 *)
    Printf.eprintf "Solved by unit propagation\n";

  let ret = Solver.solve oc in

  let end_time = Unix.gettimeofday () in
  (* Printf.eprintf "Parse time %f\n" (parse_time -. start_time); *)
  (* Printf.eprintf "Solve time %f\n" (end_time -. parse_time); *)

  Solver.printStats (end_time -. start_time);

  Printf.eprintf "\n%s\n%!" (if ret then "SATISFIABLE" else "UNSATISFIABLE");

  close_out oc;

  (* *)
