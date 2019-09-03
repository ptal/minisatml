  type 'a t = {
    mutable data : 'a array;
    mutable size : int;
    mutable cap : int;
  }

exception Return
let grow t min_cap dummy =
  let old_cap = t.cap in
 try if min_cap <= t.cap then raise Return
    else if t.cap = 0 then
      t.cap <- if min_cap >= 2 then min_cap else 2
    else begin
      let rec dowhile () =
        t.cap <- (t.cap * 3 + 1) lsr 1;
        if t.cap < min_cap then dowhile ()
      in dowhile ()
    end;
    t.data <- Array.append t.data (Array.make (t.cap - old_cap) dummy)
  with Return -> ()

  let growTo t size dummy =
    if t.size >= size then ()
    else begin
      grow t size dummy;
      t.size <- size
    end
  let init i dummy =
    let t = {data = Array.make 0 dummy;size = 0;cap = 0} in
    growTo t i dummy;
    t
  let size t = t.size
  let shrink t i = t.size <- t.size - i
  let pop t = t.size <- t.size - 1
  let push t elem dummy =
    if t.size = t.cap then begin
      let old_cap = t.cap in
      t.cap <- max 2 ((t.cap * 3 + 1) lsr 1);
      t.data <- Array.append t.data (Array.make (t.cap - old_cap) dummy)
    end;
    t.data.(t.size) <- elem;
    t.size <- t.size + 1
  let capacity t = t.cap
  let last t = t.data.(t.size-1)
  let get t i = t.data.(i)
  let get_data t = t.data
  let set t i v = t.data.(i) <- v

  let selectionSort t = ()

  (* exception Sort *)
  let sort f t =
(*  (\*    if t.size > 15 then selectionSort t *\) *)
(* (\*     else *\) *)
(* (\*       let pivot = t.data.(t.size / 2) in *\) *)
(* (\*       let i = ref (-1) in *\) *)
(* (\*       let j = ref t.size in *\) *)

(* (\*       while true do *\) *)

(* (\*         raise Sort *\) *)
(* (\*       done; *\) *)
(* (\* () *\) *)
    Array.sort f t.data

  let iter f a =
    for i = 0 to a.size -1 do
      f a.data.(i)
    done
  let iteri f a =
    for i = 0 to a.size -1 do
      f i a.data.(i)
    done
  let find t v =
    let j = ref 0 in
    while !j < size t && t.data.(!j) <> v do incr j done;
    !j < size t
    (* Array.exists (fun w -> v = w) t.data *)

  let remove t v =
    let j = ref 0 in
    while get t !j <> v && !j < size t do
      incr j;
    done;
    assert (!j < size t);
    while !j < (size t) - 1 do
      set t !j (get t (!j + 1));
    done;
    pop t
    (* let j = ref 0 in *)
    (* while get t !j <> v && !j < size t do *)
    (*   incr j; *)
    (* done; *)
    (* assert (!j < size t); *)
    (* set t !j (last t); *)
    (* pop t *)

  let clear t dummy =
    t.data <- Array.make 0 dummy;
    t.size <- 0;
    t.cap <- 0

  let copyTo from copy dummy =
    clear copy dummy;
    growTo copy (size from) dummy;
    for i = 0 to (size from) -1 do
      set copy i (get from i)
    done

  let fromList l sz =
    {data = Array.of_list l; size = sz; cap = sz}
