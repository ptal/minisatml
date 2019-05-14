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

let usage = "usage: satml [options] file.<cnf>"

let verbosity = ref 0
let debug = ref false
let trace = ref false

let spec = [
  "-verbosity",
  Arg.Set_int verbosity,
  " verbosity";

  "-debug",
  Arg.Set debug,
  " debug";

  "-trace",
  Arg.Set trace,
  " tracing decisions";
]

let parse_cmdline_arguments () =
  let ofile = ref None in
  let set_file s = ofile := Some s in
  Arg.parse spec set_file usage;
  match !ofile with
  | Some f -> f
  | None -> assert false

let main =
  (* Gc.set { (Gc.get()) with Gc.minor_heap_size = 25600000 }; *)

  let file = parse_cmdline_arguments () in
  Solver.set_verbosity !verbosity;
  Solver.set_debug !debug;
  Solver.set_trace !trace;

  let oc = open_out "./trace_decision_satml" in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _signum -> Printf.eprintf "\nTERMINATED\n%!";Solver.printStats 0.;close_out oc;exit 2));

  (* Set time counter *)
  let start_time = Unix.gettimeofday () in


  if !verbosity >= 1 then begin
    Printf.eprintf
      "============================[ Problem Statistics ]=============================\n%!";
    Printf.eprintf
      "|                                                                             |\n%!";
  end;

  let clauses, nbvars, nbcls = Parser.parse file in

  if !verbosity >= 1 then begin
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

  if !verbosity >= 1 then
    Solver.printStats (end_time -. start_time);

  Printf.eprintf "%s\n%!" (if ret then "sat" else "unsat");

  (* let stat = Gc.stat () in
   * Printf.eprintf "Max words : %d\n" stat.live_words;
   * Printf.eprintf "top heap words : %d\n" stat.top_heap_words; *)

  close_out oc;

  (* *)
