open Vec
open Types
open Heap

type var = int
type polarity = PTrue | PFalse | PUser | PRnd
let polarity_to_bool p =
  match p with
  | PTrue -> true
  | PFalse -> false
  | _ -> assert false
let bool_to_polarity b = if b then PTrue else PFalse

exception Break

type env = {
  model : Lbool.t Vec.t;
  conflict : Lit.t Vec.t;
  clauses : Clause.t Vec.t;
  learnts : Clause.t Vec.t;
  activity : float Vec.t;
  watches : Clause.t Vec.t Vec.t;
  assigns : Lbool.t Vec.t;
  polarity : polarity Vec.t;
  decision_var : bool Vec.t;
  trail : Lit.t Vec.t;
  trail_lim : int Vec.t;
  reason : Clause.t Vec.t;
  level : int Vec.t;

  seen : bool Vec.t;
  analyze_stack : Lit.t Vec.t;
  analyze_toclear : Lit.t Vec.t;
  add_tmp : Lit.t Vec.t;

  assumptions : Lit.t Vec.t;
  order_heap : var Heap.t;

  mutable var_decay : float;
  mutable clause_decay : int;
  mutable random_var_freq : float;
  mutable restart_first : int;
  mutable restart_inc : float;
  mutable learntsize_factor : float;
  mutable learntsize_inc : float;
  mutable expensive_ccmin : bool;
  mutable polarity_mode : polarity;
  mutable verbosity : int;

  mutable starts : int;
  mutable decisions : int;
  mutable rnd_decisions : int;
  mutable propagations : int;
  mutable conflicts : int;
  mutable clauses_literals : int;
  mutable learnts_literals : int;
  mutable max_literals : int;
  mutable tot_literals : int;
  mutable cla_inc : int;
  mutable var_inc : float;
  mutable ok : bool;
  mutable qhead : int;
  mutable simpDB_assigns : int;
  mutable simpDB_props : int;
  mutable random_seed : float;
  mutable progress_estimate : float;
  mutable remove_satisfied : bool;
}


let dummy_lit = Lit.lit (-1) false
let dummy_lbool = Lbool.LUndef
let dummy_clause = {
  Clause.size = 0;
  Clause.act = false;
  Clause.data = Array.make 0 dummy_lit;}
let dummy_polarity = PTrue
let dummy_var = -1

let debug = ref false
let trace = ref false

let env = {
  model = Vec.init 0 dummy_lbool;
  conflict = Vec.init 0 dummy_lit;
  clauses = Vec.init 0 dummy_clause;
  learnts = Vec.init 0 dummy_clause;
  activity = Vec.init 0 0.;
  watches = Vec.init 0 (Vec.init 0 dummy_clause);
  assigns = Vec.init 0 dummy_lbool;
  polarity = Vec.init 0 dummy_polarity;
  decision_var = Vec.init 0 false;
  trail = Vec.init 0 dummy_lit;
  trail_lim = Vec.init 0 0;
  reason = Vec.init 0 dummy_clause;
  level = Vec.init 0 0;

  seen = Vec.init 0 false;
  analyze_stack = Vec.init 0 dummy_lit;
  analyze_toclear = Vec.init 0 dummy_lit;
  add_tmp = Vec.init 0 dummy_lit;

  assumptions = Vec.init 0 dummy_lit;
  order_heap = Heap.init dummy_lit;

  var_decay = 1. /. 0.95;
  clause_decay = 1;
  random_var_freq = 0.02;
  restart_first = 100;
  restart_inc = 1.5;
  learntsize_factor = 1. /. 30.;
  learntsize_inc = 1.1;
  expensive_ccmin = false;
  polarity_mode = dummy_polarity;
  verbosity = 0;

  starts = 0;
  decisions = 0;
  rnd_decisions = 0;
  propagations = 0;
  conflicts = 0;
  clauses_literals = 0;
  learnts_literals = 0;
  max_literals = 0;
  tot_literals = 0;
  cla_inc = 1;
  var_inc = 1.;
  ok = true;
  qhead = 0;
  simpDB_assigns = -1;
  simpDB_props = 0;
  random_seed = 91648253.;
  progress_estimate = 0.;
  remove_satisfied = true;
}

let set_debug b =
  debug := b

let set_trace b =
  trace := b

let set_verbosity n =
  env.verbosity <- n

let heap_comp i j = (Vec.get env.activity i) > (Vec.get env.activity j)


let value_of_int v = Vec.get env.assigns v
let value l = if Lit.sign l then Vec.get env.assigns (Lit.var l) else Lbool.inv (Vec.get env.assigns (Lit.var l))


let printLit l =
  Printf.sprintf "%s%d:%c%!"
    (if Lit.sign l then "-" else "")
    ((Lit.var l) + 1)
    (if value l = Lbool.LTrue then '1' else if value l = Lbool.LFalse then '0' else 'X')


let printClause c =
  Printf.eprintf "{ %!";
  for i = 0 to Clause.size c - 1 do
    Printf.eprintf "%s ," (    printLit (Clause.get c i))
  done;
  Printf.eprintf " }%!"



let locked c = (value (Clause.get c 0)) = Lbool.LTrue && (Vec.get env.reason (Lit.var (Clause.get c 0))) == c
let newDecisionLevel () = Vec.push env.trail_lim (Vec.size env.trail) 0

let decisionLevel () = Vec.size env.trail_lim
let abstractLevel v = ((Vec.get env.level v) land 31) lsl 1
let modelValue l =
  (* Printf.eprintf "lit %s (%b)" (printLit l) (Lit.sign l); *)
  (* Lbool.chap (Vec.get env.model (Lit.var l)) (Lit.sign l) *)

if Lit.sign l then Vec.get env.model (Lit.var l) else Lbool.inv ((Vec.get env.model (Lit.var l)))


let nAssigns () = Vec.size env.trail
let nClauses () = Vec.size env.clauses
let nLearnts () = Vec.size env.learnts
let nVars () = Vec.size env.assigns

let insertVarOrder x =
  if not (Heap.inHeap env.order_heap x) && Vec.get env.decision_var x then Heap.insert heap_comp env.order_heap x

let varDecayActivity () =
  env.var_inc <- env.var_inc *. env.var_decay

let varBumpActivity v =
  Vec.set env.activity v ((Vec.get env.activity v) +. env.var_inc);
  if (Vec.get env.activity v) > (1e100) then begin
    (* Rescale *)
    for i = 0 to nVars () - 1 do
      Vec.set env.activity i ((Vec.get env.activity i) *. (1e-100));
    done;
    env.var_inc <- env.var_inc *. (1e-100)
  end;

  (* Update order_heap with respect to new activity: *)
  if Heap.inHeap env.order_heap v then
    (*     if (false) printf ("decrease %d with new weight %f\n", (v+1), activity[v]);*)
    Heap.decrease heap_comp env.order_heap v


let claDecayActivity () =
  env.cla_inc <- env.cla_inc * env.clause_decay
let claBumpActivity c =
  Clause.set_activity c ((Clause.get_activity c) + env.cla_inc);
  if Clause.get_activity c > (int_of_float 1e20) then begin
    for i = 0 to Vec.size env.learnts - 1 do
      Clause.set_activity (Vec.get env.learnts i) (Clause.get_activity (Vec.get
  env.learnts i) * (int_of_float 1e-20))
    done;
    env.cla_inc <- env.cla_inc * (int_of_float 1e-20)
  end

let pickBranchLit p v =
  let next = ref dummy_var in
  begin try
      while !next = dummy_var ||
            Vec.get env.assigns !next <> Lbool.LUndef ||
            not (Vec.get env.decision_var !next) do
        if Heap.empty env.order_heap then begin
          next := dummy_var;
          raise Break;
        end
        else
          next := Heap.removeMin heap_comp env.order_heap
      done;
    with Break -> () end;
  let l = if !next = dummy_var then dummy_lit else Lit.lit !next false in
  if !debug && !next <> dummy_var then begin
    Printf.eprintf "picked %s from:\n%!" (printLit l);
    for i = 0 to Vec.size env.activity -1 do
      Printf.eprintf "%d -> %f\n%!" (i+1) (Vec.get env.activity i)
    done;
    Printf.eprintf "------------------------\n\n";
  end;
  l


let cancelUntil level =
  (* Printf.eprintf " Cancel until %d\n%!" level; *)
  if decisionLevel () > level then begin
    for c = (Vec.size env.trail) -1 downto Vec.get env.trail_lim level do
      let x  = Lit.var (Vec.get env.trail c) in
      Vec.set env.assigns x Lbool.LUndef;
      insertVarOrder x
    done;
    env.qhead <- Vec.get env.trail_lim level;
    Vec.shrink env.trail ((Vec.size env.trail) - (Vec.get env.trail_lim level));
    Vec.shrink env.trail_lim ((Vec.size env.trail_lim) - level)
  end

let setPolarity v b = Vec.set env.polarity v (bool_to_polarity b)
let setDecisionVar v b =
  Vec.set env.decision_var v b;
  if b then insertVarOrder v

let okay () = env.ok


let newVar_var sign dvar =
  let v = nVars () in
  Vec.push env.watches (Vec.init 0 dummy_clause) (Vec.init 0 dummy_clause);
  Vec.push env.watches (Vec.init 0 dummy_clause) (Vec.init 0 dummy_clause);
  Vec.push env.reason dummy_clause dummy_clause;
  Vec.push env.assigns dummy_lbool dummy_lbool;
  Vec.push env.level (-1) (-1);
  Vec.push env.activity 0. 0.;
  Vec.push env.seen false false;
  Vec.push env.polarity (bool_to_polarity sign) dummy_polarity;
  Vec.push env.decision_var dvar dvar;
  insertVarOrder(v);
  v

let newVar () =
  newVar_var true true

let uncheckedEnqueue_clause p from =
  assert ((value p) = Lbool.LUndef);
  Vec.set env.assigns (Lit.var p) (Lbool.lbool_of_bool (Lit.sign p));
  Vec.set env.level (Lit.var p) (decisionLevel ());
  Vec.set env.reason (Lit.var p) from;
  Vec.push env.trail p dummy_lit;
  if !debug then
    Printf.eprintf "I enqueue the atom %s\n%!" (printLit p)

let uncheckedEnqueue l =
  let from = dummy_clause in
  uncheckedEnqueue_clause l from

(* let enqueue_clause p from = *)
(*   if (value p) <> Lbool.LUndef then *)
(*     if  *)
(*   if not (Lit.neq p dummy_lit) then *)
(*     Lbool.lbool_of_bool *)
(*       (Lit. l <> Lbool.LFalse) *)
(*   else begin *)
(*     uncheckedEnqueue_clause l c; *)
(*     Lbool.LTrue *)
(*   end *)

(* let enqueue p = *)
(*   let from = dummy_clause in *)
(*   enqueue_clause p from *)


exception FoundWatch
(*____________________________________________________________________________
  |
  |  propagate : [void]  ->  [Clause*]
  |
  |  Description:
  |    Propagates all enqueued facts. If a conflict arises, the conflicting clause is returned,
  |    otherwise NULL.
  |
  |    Post-conditions:
  |      * the propagation queue is empty, even if there was a conflict.
  |_____________________________________________________________________________*)
let propagate () =
  let confl = ref dummy_clause in
  let num_props = ref 0 in

  while env.qhead < (Vec.size env.trail) do
    (* 'p' is enqueued fact to propagate. *)
    let p = Vec.get env.trail env.qhead in
    env.qhead <- env.qhead + 1;

    let ws = Vec.get env.watches (Lit.toInt p) in
    incr num_props;

    let j = ref 0 in
    let i = ref 0 in
    let end_var = Vec.size ws in

    if !debug then begin
      Printf.eprintf "I propagate the atom %s\n%s watches the following clauses:\n%!" (printLit p) (printLit p);
      for i = 0 to Vec.size ws -1 do
        (* Printf.eprintf " >%s:{ %!" (if Clause.learnt (Vec.get ws i) then "L" else "C"); *)
        for j = 0 to Clause.size (Vec.get ws i) -1 do
          Printf.eprintf "%s ; " (printLit (Clause.get (Vec.get ws i) j));
        done;
        Printf.eprintf "}\n%!";
      done
    end;

    while !i <> end_var do
      let c = Vec.get ws !i in
      incr i;

      if !debug then begin
        (* Printf.eprintf "Propagate in clause %s:{ %!" (if Clause.learnt c then "L" else "C"); *)
        Clause.iter (fun l ->
            Printf.eprintf "%s ; %!" (printLit l)
          ) c;
        Printf.eprintf "}\n%!"
      end;

      (* Make sure the false literal is data[1]: *)
      let false_lit = Lit.tild p in
      if Clause.get c 0 = false_lit then begin
        Clause.set c 0 (Clause.get c 1);
        Clause.set c 1 false_lit;
      end;

      assert (Clause.get c 1 = false_lit);

      (* If 0th watch is true, then clause is already satisfied. *)
      let first = Clause.get c 0 in
      if value first = Lbool.LTrue then begin
        Vec.set ws !j c;
        incr j;
      end
      else begin
        try
          (* Look for new watch: *)
          for k = 2 to (Clause.size c) - 1 do
            if value (Clause.get c k) <> Lbool.LFalse then begin
              Clause.set c 1 (Clause.get c k);
              Clause.set c k false_lit;
              Vec.push (Vec.get env.watches (Lit.toInt (Lit.tild (Clause.get c 1)))) c dummy_clause;
              raise FoundWatch
            end;
          done;

          (* Did not find watch -- clause is unit under assignment: *)
          Vec.set ws !j c;
          incr j;
          if value first = Lbool.LFalse then begin
            confl := c;
            env.qhead <- Vec.size env.trail;
            (* Copy the remaining watches *)
            while !i < end_var do
              Vec.set ws !j (Vec.get ws !i);
              incr j;
              incr i;
            done
          end
          else begin
            uncheckedEnqueue_clause first c
          end
        with FoundWatch -> ()
      end;
    done;
    let dead_part = !i - !j in
    Vec.shrink (Vec.get env.watches (Lit.toInt p)) dead_part;
    if !debug then begin
      Printf.eprintf "shrink  %d elements\n%s watches the following clauses:\n%!" dead_part (printLit p);
      Vec.iter (fun c ->
          (* Printf.eprintf " >%s:{ %!" (if Clause.learnt c then "L" else "C"); *)
          Clause.iter (fun l ->
              Printf.eprintf "%s ; %!" (printLit l)
            ) c;
          Printf.eprintf "}\n%!"
        ) ws;
    end
  done;
  env.propagations <- env.propagations + !num_props;
  env.simpDB_props <- env.simpDB_props - !num_props;
  !confl

let attachClause c =
  assert (Clause.size c > 1);
  Vec.push (Vec.get env.watches (Lit.toInt (Lit.tild (Clause.get c 0)))) c dummy_clause;
  Vec.push (Vec.get env.watches (Lit.toInt (Lit.tild (Clause.get c 1)))) c dummy_clause;
  if Clause.learnt c then
    env.learnts_literals <- env.learnts_literals + (Clause.size c)
  else
    env.clauses_literals <- env.clauses_literals + (Clause.size c)


exception Return of bool
let addClause ps =
  assert (decisionLevel () = 0);
  if not env.ok then false
  else begin
    (* Check if clause is satisfied and remove false/duplicate literals: *)
    Vec.sort compare ps;
    let j = ref 0 in
    let i = ref 0 in
    let p = ref dummy_lit in
    try
      while !i < Vec.size ps do
        if value (Vec.get ps !i) = Lbool.LTrue || (Vec.get ps !i) = (Lit.tild !p)
        then raise (Return true)
        else if value (Vec.get ps !i) <> Lbool.LFalse && (Vec.get ps !i) <> !p
        then begin
          p := Vec.get ps !i;
          Vec.set ps !j !p;
          incr j;
        end;
        incr i
      done;
      Vec.shrink ps (!i - !j);
      match Vec.size ps with
      | 0 -> env.ok <- false;false
      | 1 ->
        assert (value (Vec.get ps 0) = Lbool.LUndef);
        uncheckedEnqueue (Vec.get ps 0);
        let ok = propagate () = dummy_clause in
        env.ok <- ok; ok
      | n ->
        let c = Clause.clause_new (Vec.get_data ps) (Vec.size ps) false in
        Vec.push env.clauses c dummy_clause;
        attachClause c;

        if !debug then begin
          Printf.eprintf "I add a clause: C:{ %!";
          for i = 0 to (Clause.size c) -1 do
            Printf.eprintf "%s ; %!" (printLit (Clause.get c i));
          done;
          Printf.eprintf "}\n%!";
        end;
        true
    with Return b -> b;
  end

let detachClause c =
  assert (Clause.size c > 1);
  assert (Vec.find (Vec.get env.watches (Lit.toInt (Lit.tild (Clause.get c 0)))) c);
  assert (Vec.find (Vec.get env.watches (Lit.toInt (Lit.tild (Clause.get c 1)))) c);
  Vec.remove (Vec.get env.watches (Lit.toInt (Lit.tild (Clause.get c 0)))) c;
  Vec.remove (Vec.get env.watches (Lit.toInt (Lit.tild (Clause.get c 1)))) c;
  if Clause.learnt c then
    env.learnts_literals <- env.learnts_literals - (Clause.size c)
  else
    env.clauses_literals <- env.clauses_literals - (Clause.size c)

let removeClause c =
  if !debug then begin
    Printf.eprintf "I remove the clause: %s:{ " (if Clause.learnt c then "L" else "C");
    for i = 0 to (Clause.size c) -1 do
      Printf.eprintf "%s ; %!" (printLit (Clause.get c i));
    done;
    Printf.eprintf "\n%!";
  end;
  detachClause c

let satisfied c =
  let res = ref false in
  Clause.iter (fun v -> if value v = Lbool.LTrue then res := true;) c;
  !res


exception Next

let verifyModel () =
  let failed = ref false in
  begin try
      Vec.iter (fun c ->
          (* assert (Clause.mark c = 0); *)
          Clause.iter ( fun l ->
              if (modelValue l) = Lbool.LTrue then
                raise Next
            ) c;
          Printf.eprintf "unsatisfied clause : ";
          printClause c;
          Printf.eprintf "\n";
          failed := true
        ) env.clauses;
    with Next -> ();
  end;

  assert (not !failed);
  if !debug then Printf.eprintf "Verified %d original clauses.\n%!" (Vec.size env.clauses)


let checkLiteralCount () =
  let cnt = ref 0 in
  Vec.iter (fun c ->
      (* if Clause.mark c = 0 then *)
      cnt := !cnt + Clause.size c
    ) env.clauses;

  if env.clauses_literals <> !cnt then begin
    Printf.eprintf "literal count: %d, real value = %d\n" env.clauses_literals !cnt;
    assert (env.clauses_literals = !cnt);
  end

(*________________________________________________________________________
  |
  |  analyze : (confl : Clause* ) (out_learnt : vec<Lit>&) (out_btlevel : int&)  ->  [void]
  |
  |  Description:
  |    Analyze conflict and produce a reason clause.
  |
  |    Pre-conditions:
  |      * 'out_learnt' is assumed to be cleared.
  |      * Current decision level must be greater than root level.
  |
  |    Post-conditions:
  |      * 'out_learnt[0]' is the asserting literal at level 'out_btlevel'.
  |
  |  Effect:
  |    Will undo part of the trail, upto but not beyond the assumption of the current decision level.
  |__________________________________________________________________________ *)
let analyze confl out_learnt out_btlevel =
  let pathC = ref 0 in
  let p = ref dummy_lit in

  (* Generate conflict clause: *)

  (* Leave room for the asserting literal *)
  Vec.push out_learnt dummy_lit dummy_lit;
  let index = ref ((Vec.size env.trail) - 1) in
  out_btlevel := 0;

  let rec dowhile () =
    if !debug then begin
      Printf.eprintf "\nI analyze the clause %s:{ " (if Clause.learnt !confl then"L" else "C");
      for i = 0 to Clause.size !confl -1 do
        Printf.eprintf "%s ; " (printLit (Clause.get !confl i));
      done;
      Printf.eprintf "}\n%!";
    end;

    assert (!confl <> dummy_clause);

    if Clause.learnt !confl then
        claBumpActivity !confl;

    for j = (if !p = dummy_lit then 0 else 1) to Clause.size !confl -1 do
      let q = Clause.get !confl j in

      if not (Vec.get env.seen (Lit.var q)) && Vec.get env.level (Lit.var q) > 0 then begin
        varBumpActivity (Lit.var q);
        Vec.set env.seen (Lit.var q) true;
        if Vec.get env.level (Lit.var q) >= decisionLevel () then
          incr pathC
        else begin
          Vec.push out_learnt q dummy_lit;
          if Vec.get env.level (Lit.var q) > !out_btlevel then
            out_btlevel := Vec.get env.level (Lit.var q);
        end
      end
    done;
    (* Select next clause to look at: *)
    while not (Vec.get env.seen (Lit.var (Vec.get env.trail !index))) do
      decr index
    done;
    p := Vec.get env.trail !index;
    confl := Vec.get env.reason (Lit.var !p);
    Vec.set env.seen (Lit.var !p) false;
    decr pathC;

    if !pathC > 0 then
      dowhile ()
  in
  dowhile ();
  Vec.set out_learnt 0 (Lit.tild !p);
  Vec.copyTo out_learnt env.analyze_toclear dummy_lit;
  for j = 0 to Vec.size env.analyze_toclear -1 do
    Vec.set env.seen (Lit.var (Vec.get env.analyze_toclear j)) false
  done

(* Check if 'p' can be removed. 'abstract_levels' is used to abort early if the algorithm is visiting literals at levels that cannot be removed later. *)
let litRedundant p abstract_levels =
  Vec.clear env.analyze_stack dummy_lit;
  Vec.push env.analyze_stack p dummy_lit;
  let top = Vec.size env.analyze_stack in
  try
    while Vec.size env.analyze_stack > 0 do
      assert (Vec.get env.reason (Lit.var (Vec.last env.analyze_stack)) <> dummy_clause);
      let c = Vec.get env.reason (Lit.var (Vec.last env.analyze_stack)) in
      Vec.pop env.analyze_stack;

      for i = 1 to Clause.size c - 1 do
        let p = Clause.get c i in
        if not (Vec.get env.seen (Lit.var p)) && Vec.get env.level (Lit.var p) > 0 then begin
          if Vec.get env.reason (Lit.var p) <> dummy_clause && ((abstractLevel (Lit.var p)) land abstract_levels) <> 0 then begin
            Vec.set env.seen (Lit.var p) true;
            Vec.push env.analyze_stack p dummy_lit;
            Vec.push env.analyze_toclear p dummy_lit
          end
          else begin
            for j = top to Vec.size env.analyze_toclear -1 do
              Vec.set env.seen (Lit.var (Vec.get env.analyze_toclear j)) false
            done;
            Vec.shrink env.analyze_toclear (Vec.size env.analyze_toclear - top);
            raise (Return false)
          end
        end
      done;
    done;
    true
  with Return b -> b

let removeSatisfied cs =
  let j = ref 0 in
  let i = ref 0 in
  while !i < Vec.size cs - 1 do
    let c = Vec.get cs !i in
    if !debug then begin
      Printf.eprintf "Remove %s:{ " (if Clause.learnt c then "L" else "C");
      Clause.iter (fun l -> Printf.eprintf "%s ; " (printLit l)) c;
      Printf.eprintf "} ?\n%!";
    end;
    if satisfied c then
      removeClause c
    else begin
      Vec.set cs !j c;
      incr j;
    end;
    incr i
  done;
  Vec.shrink cs (Vec.size cs - !j)

(*____________________________________________________________________________
  |
  |  simplify : [void]  ->  [bool]
  |
  |  Description:
  |    Simplify the clause database according to the current top-level assigment. Currently, the only
  |    thing done here is the removal of satisfied clauses, but more things can be put here.
  |_____________________________________________________________________________*)
let simplify () =
  (* assert (decisionLevel () = 0); *)
  (* if not (okay ()) || propagate () <> dummy_clause then *)
  (*   okay () = false *)
  (* else if nAssigns () = env.simpDB_assigns || env.simpDB_props > 0 then true *)
  (* else begin *)
  (*   removeSatisfied env.learnts; *)
  (*   if env.remove_satisfied then removeSatisfied env.clauses; *)
  (*   (\* Heap.filter env.order_heap  *\) *)
  (*   env.simpDB_assigns <- nAssigns (); *)
  (*   env.simpDB_props <- env.clauses_literals + env.learnts_literals; *)
  (*   true *)
  (* end *)
  true

let progressEstimate () =
  let progress = ref 0. in
  let f = 1. /. (float_of_int (nVars ())) in

  for i = 0 to decisionLevel () do
    let begg =  if i = 0 then 0 else Vec.get env.trail_lim (i - 1) in
    let endd = if i = (decisionLevel ()) then Vec.size env.trail else Vec.get env.trail_lim i in
    progress := !progress +. ((f ** (float_of_int i)) *. (float_of_int (endd - begg)));
  done;

  !progress /. (float_of_int (nVars ()))

(*_____________________________________________________________________________
  |
  |  reduceDB : ()  ->  [void]
  |
  |  Description:
  |    Remove half of the learnt clauses, minus the clauses locked by the current assignment. Locked
  |    clauses are clauses that are reason to some assignment. Binary clauses are never removed.
  |______________________________________________________________________________*)
let reduceDB () = (* assert false *) ()

(*______________________________________________________________________________
  |
  |  analyzeFinal : (p : Lit)  ->  [void]
  |
  |  Description:
  |    Specialized analysis procedure to express the final conflict in terms of assumptions.
  |    Calculates the (possibly empty) set of assumptions that led to the assignment of 'p', and
  |    stores the result in 'out_conflict'.
  |_____________________________________________________________________________*)
let analyzeFinal p out_conflict =
  Vec.clear out_conflict dummy_lit;
  Vec.push out_conflict p dummy_lit;

  if decisionLevel () = 0 then
    ()
  else begin
    Vec.set env.seen (Lit.var p) true;

    for i = ((Vec.size env.trail) -1) downto Vec.get env.trail_lim 0 do
      let x = Lit.var (Vec.get env.trail i) in
      if Vec.get env.seen x then begin
        if Vec.get env.reason x = dummy_clause then begin
          assert (Vec.get env.level x > 0);
          Vec.push out_conflict (Lit.tild (Vec.get env.trail i)) dummy_lit
        end else begin
          let c = Vec.get env.reason x in
          for j = 1 to Clause.size c -1 do
            if Vec.get env.level (Lit.var (Clause.get c j)) > 0 then
              Vec.set env.seen (Lit.var (Clause.get c j)) true
          done;
        end;
        Vec.set env.seen x false
      end
    done;
    Vec.set env.seen (Lit.var p) false;
  end


exception Search of Lbool.t
(*______________________________________________________________________________
  |
  |  search : (nof_conflicts : int) (nof_learnts : int) (params : const SearchParams&)  ->  [lbool]
  |
  |  Description:
  |    Search for a model the specified number of conflicts, keeping the number of learnt clauses
  |    below the provided limit. NOTE! Use negative value for 'nof_conflicts' or 'nof_learnts' to
  |    indicate infinity.
  |
  |  Output:
  |    'l_True' if a partial assigment that is consistent with respect to the clauseset is found. If
  |    all variables are decision variables, this means that the clause set is satisfiable. 'l_False'
  |    if the clause set is unsatisfiable. 'l_Undef' if the bound on number of conflicts is reached.
  |_____________________________________________________________________________*)
let search oc nof_conflicts nof_learnts =
  assert env.ok;
  let backtrack_level = ref 0 in
  let conflictC = ref 0 in
  let learnt_clause = Vec.init 0 dummy_lit in

  env.starts <- env.starts + 1;

  let first = ref true in

  try while true do
      let confl = ref (propagate ()) in
      if !confl <> dummy_clause then begin
        (* Conflict *)
        env.conflicts <- env.conflicts + 1;
        incr conflictC;

        if !debug then
          Printf.eprintf "I have a (%d-th) conflict at level %d\n%!" !conflictC (decisionLevel ());

        if decisionLevel () = 0 then raise (Search Lbool.LFalse);

        first := false;

        Vec.clear learnt_clause dummy_lit;
        analyze confl learnt_clause backtrack_level;
        cancelUntil !backtrack_level;
        assert ((value (Vec.get learnt_clause 0)) = Lbool.LUndef);

        if !debug then begin
          Printf.eprintf "Backjump to level %d and learn the clause { %!" !backtrack_level;
          for i = 0 to Vec.size learnt_clause - 1 do
            Printf.eprintf "%s ; %!" (printLit (Vec.get learnt_clause i))
          done;
          Printf.eprintf "}\n%!"
        end;

        if Vec.size learnt_clause = 1 then
          uncheckedEnqueue (Vec.get learnt_clause 0)
        else begin
          let c = Clause.clause_new (Vec.get_data learnt_clause) (Vec.size learnt_clause) true in
          Vec.push env.learnts c dummy_clause;
          attachClause c;
          claBumpActivity c;
          uncheckedEnqueue_clause (Vec.get learnt_clause 0) c
        end;

        varDecayActivity ();
        claDecayActivity ()
      end
      else begin
        (* No Conflict *)
        (* Printf.eprintf " nof conflic : %d, conflictc : %d\n%!" nof_conflicts !conflictC; *)
        if nof_conflicts >= 0 && !conflictC >= nof_conflicts then begin
          (* Printf.eprintf " Max conflict\n%!"; *)
          (* Reached bound on number of conflicts: *)
          env.progress_estimate <- progressEstimate ();
          cancelUntil 0;
          raise (Search Lbool.LUndef)
        end;

        (* Simplify the set of problem clauses: *)
        if decisionLevel () = 0 && not (simplify ()) then raise (Search Lbool.LFalse);

        if nof_learnts >= 0 && (Vec.size env.learnts) - nAssigns () >= nof_learnts then
          (* Reduce the set of learnt clauses: *)
          reduceDB ();

        let next = ref dummy_lit in
        begin try
            while decisionLevel() < Vec.size env.assumptions do
              (* Perform user provided assumption: *)
              let p = Vec.get env.assumptions (decisionLevel ()) in
              if value p = Lbool.LTrue then
                (* Dummy decision level: *)
                newDecisionLevel ()
              else if value p = Lbool.LFalse then begin
                analyzeFinal (Lit.tild p) env.conflict;
                raise (Search Lbool.LFalse)
              end else begin
                next := p;
                raise Break
              end
            done
          with Break -> ()
        end;
        if !next = dummy_lit then begin
          (*New variable decision: *)
          env.decisions <- env.decisions + 1;
          next := pickBranchLit env.polarity_mode env.random_var_freq;
          if !next = dummy_lit then
            (* Model found *)
            raise (Search Lbool.LTrue)
        end;
        (* Increase decision level and enqueue next *)
        assert (value !next = Lbool.LUndef);
        newDecisionLevel ();

        if !trace then
          Printf.fprintf oc "%s\n" (printLit !next);

        if !debug then
          Printf.eprintf "(%dth) I decide the atom %s at level %d\n" env.decisions (printLit !next) (decisionLevel ());

        uncheckedEnqueue !next;
      end
    done;
    Lbool.LUndef
  with Search b -> b


let solve_lit oc assumps =
  Vec.clear env.model dummy_lbool;
  Vec.clear env.conflict dummy_lit;


  if not env.ok then false
  else begin
    Vec.copyTo assumps env.assumptions dummy_lit;

    let nof_conflicts = ref (float_of_int env.restart_first) in
    let nof_learnts = ref ((float_of_int (nClauses ())) *. env.learntsize_factor) in
    let status = ref Lbool.LUndef in

    if not !debug && env.verbosity >= 1 then
      Printf.eprintf
        "============================[ Search Statistics ]==============================\n| Conflicts |          ORIGINAL         |          LEARNT          | Progress |\n|           |    Vars  Clauses Literals |    Limit  Clauses Lit/Cl |          |\n===============================================================================\n%!";

    (* Search *)
    while !status = Lbool.LUndef do
      if not !debug && env.verbosity >= 1 then
        Printf.eprintf "| %9d | %7d %8d %8d | %8d %8d %6.0f | %6.3f %% |\n%!"
          env.conflicts
          (Heap.size env.order_heap)
          (nClauses ())
          env.clauses_literals
          (int_of_float !nof_learnts)
          (nLearnts ())
          (try (float_of_int (env.learnts_literals/(nLearnts()))) with e -> 0.)
          (env.progress_estimate*.100.);
      status := search oc (int_of_float !nof_conflicts) (int_of_float !nof_learnts);
      (* Printf.eprintf "end Search\n"; *)
      nof_conflicts := !nof_conflicts *. env.restart_inc;
      nof_learnts := !nof_learnts *. env.learntsize_inc;
    done;
    if not !debug && env.verbosity >= 1 then
      Printf.eprintf "===============================================================================\n";

    if !status = Lbool.LTrue then begin
      (* Extend & copy model *)
      Vec.grow env.model (nVars ()) dummy_lbool;
      for i = 0 to nVars () - 1 do
        Vec.set env.model i (value i)
      done;
      verifyModel ()
    end
    else begin
      assert (!status = Lbool.LFalse);
      if Vec.size env.conflict = 0 then
        env.ok <- false
    end;

    cancelUntil 0;
    !status = Lbool.LTrue
  end

let solve oc =
  let tmp = Vec.init 0 dummy_lit in
  solve_lit oc tmp


let printStats cpu_time =
  if true then begin
    Printf.eprintf "restart             : %d\n%!" env.starts;
    Printf.eprintf "conflicts           : %d   (%.0f /sec)\n%!" env.conflicts ((float_of_int env.conflicts) /. cpu_time);
    Printf.eprintf "decisions           : %d   (%.0f /sec)\n%!" env.decisions ((float_of_int env.decisions) /. cpu_time);
    Printf.eprintf "propagations        : %d   (%.0f /sec)\n%!" env.propagations ((float_of_int env.propagations) /. cpu_time);
    Printf.eprintf "conflict literals   : %d   (%.0f /sec)\n%!" env.tot_literals ((float_of_int (env.max_literals - env.tot_literals)) /. (float_of_int env.max_literals));
    Printf.eprintf "CPU time            : %f s\n%!" cpu_time
  end;
