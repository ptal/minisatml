module Vec = struct
  type 'a t = {
    mutable data : 'a array;
    mutable size : int;
    mutable cap : int;
  }

  let init i dummy = {data = Array.make i dummy;size = i;cap = 0}
  let grow t i dummy = t.data <- Array.append t.data (Array.make (i - t.size) dummy); t.cap <- i
  let size t = t.size
  let shrink t i = t.size <- i
  let pop t = t.size <- t.size - 1
  let push t n dummy =
    if t.size = t.cap then begin
      grow t (2*t.cap+1) dummy;
    end;
   t.data.(t.size) <- n;
    t.size <- t.size + 1
  let capacity t = t.cap
  let last t = t.data.(t.size-1)
  let get t i = t.data.(i)
  let get_data t = t.data
  let set t i v = t.data.(i) <- v

  let selectionSort t = ()

  exception Sort
  let sort t = ()
(*  (\*    if t.size > 15 then selectionSort t *\) *)
(* (\*     else *\) *)
(* (\*       let pivot = t.data.(t.size / 2) in *\) *)
(* (\*       let i = ref (-1) in *\) *)
(* (\*       let j = ref t.size in *\) *)

(* (\*       while true do *\) *)
        
(* (\*         raise Sort *\) *)
(* (\*       done; *\) *)
(* (\* () *\) *)
(*     Array.fast_sort (fun i j -> if i < j then -1 else if j > i then 1 else 0) t.data *)

  let iter f a =
    for i = 0 to a.size -1 do
      f a.data.(i)
    done
  let iteri f a =
    for i = 0 to a.size -1 do
      f i a.data.(i)
    done
  let find t v =
    Array.exists (fun w -> v = w) t.data

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

  let clear t dummy =
    t.data <- Array.make 0 dummy;
    t.size <- 0;
    t.cap <- 0

  let copyTo from copy dummy =
    clear copy dummy;
    grow copy (size from) dummy;
    for i = 0 to (size from) -1 do
      set copy i (get from i)
    done

  let fromList l sz =
    {data = Array.of_list l; size = sz; cap = sz}

end
