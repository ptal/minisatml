module Vec_int = struct
  type t = {
    mutable data : int array;
    (* mutable size : int; *)
    (* mutable cap : int; *)
  }

let cap t = Obj.size (Obj.repr t.data) -1
let size t = t.data.(0)
let set_size t i = t.data.(0) <- i

exception Return
let grow t min_cap dummy =
  let old_cap = cap t in
  let cur_cap = ref (cap t) in
 try if min_cap <= !cur_cap then raise Return
    else if !cur_cap = 0 then
      cur_cap := if min_cap >= 2 then min_cap else 2
    else begin
      let rec dowhile () =
        cur_cap := (!cur_cap * 3 + 1) lsr 1;
        if !cur_cap < min_cap then dowhile ()
      in dowhile ()
    end;
    t.data <- Array.append t.data (Array.make (!cur_cap - old_cap) dummy)
  with Return -> ()

  let growTo t sizeTo dummy =
    if size t >= sizeTo then ()
    else begin
      grow t sizeTo dummy;
      set_size t sizeTo
    end
  let init i dummy =
    let t = {data = Array.make 1 0} in
    growTo t i dummy;
    t
  let shrink t i = set_size t (size t - i)
  let pop t = set_size t (size t - 1)
  let push t elem dummy =
    if size t = cap t then begin
      let old_cap = cap t in
      let cur_cap = max 2 ((old_cap * 3 + 1) lsr 1) in
      t.data <- Array.append t.data (Array.make (cur_cap - old_cap) dummy)
    end;
    t.data.(size t) <- elem;
    set_size t (size t + 1)

  let capacity t = Obj.size (Obj.repr t.data) -1
  let last t = t.data.(size t -1)
  let get t i = t.data.(i+1)
  let get_data t = t.data
  let set t i v = t.data.(i+1) <- v

  let selectionSort _ = ()

  (* exception Sort *)
  let sort _f _t =
(*  (\*    if t.size > 15 then selectionSort t *\) *)
(* (\*     else *\) *)
(* (\*       let pivot = t.data.(t.size / 2) in *\) *)
(* (\*       let i = ref (-1) in *\) *)
(* (\*       let j = ref t.size in *\) *)

(* (\*       while true do *\) *)

(* (\*         raise Sort *\) *)
(* (\*       done; *\) *)
(* (\* () *\) *)
    (* Array.sort f t.data *)()

  let iter f a =
    for i = 1 to size a -1 do
      f a.data.(i)
    done
  let iteri f a =
    for i = 1 to size a -1 do
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

  let clear t _dummy =
    t.data <- Array.make 1 0

  let copyTo from copy dummy =
    clear copy dummy;
    growTo copy (size from) dummy;
    for i = 1 to (size from) -1 do
      set copy i (get from i)
    done

  let fromList l sz =
    {data = Array.append (Array.make 1 sz) (Array.of_list l)}

end
