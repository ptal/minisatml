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
  let file = Sys.argv.(1) in

  let verbosity =
    try int_of_string Sys.argv.(2)
    with _ -> 0
  in
  let trace =
    try bool_of_string Sys.argv.(3)
    with _ -> false
  in
  let debug =
    try bool_of_string Sys.argv.(4)
    with _ -> false
  in
  Solver.set_verbosity verbosity;
  Solver.set_debug debug;
  Solver.set_trace trace;

  let oc = open_out "./trace_decision_satml" in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _signum -> Printf.eprintf "\nTERMINATED\n%!";Solver.printStats 0.;close_out oc;exit 2));

  (* Set time counter *)
  let start_time = Unix.gettimeofday () in


  if verbosity >= 1 then begin
    Printf.eprintf
      "============================[ Problem Statistics ]=============================\n%!";
    Printf.eprintf
      "|                                                                             |\n%!";
  end;

  let clauses, nbvars, nbcls = Parser.parse file in

  if verbosity >= 1 then begin
    Printf.eprintf
      "|  Number of variables:  %-12d                                         |\n%!"
      nbvars;
    Printf.eprintf
      "|  Number of clauses:    %-12d                                         |\n%!"
      nbcls;
  end;

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

  if verbosity >= 1 then
    Solver.printStats (end_time -. start_time);

  Printf.eprintf "%s\n%!" (if ret then "sat" else "unsat");

  close_out oc;

  (* *)
