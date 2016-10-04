open Vec

module Heap = struct
  type 'a t = {
    heap : 'a Vec.t;
    indice : int Vec.t;
  }

  let init dummy = {
    heap = Vec.init 0 dummy;
    indice = Vec.init 0 0;
  }

  let reinit h sz dummy =
    Vec.grow h.heap sz dummy;
    Vec.grow h.indice sz 0

  let left i = i*2+1
  let right i = (i+1)*2
  let parent i = (i-1) lsr 1

  (* let get t i = *)
  (*   assert (i < Vec.size t.heap); *)
  (*   Vec.get t.heap i *)

  (* let set t i v = *)
  (*   assert (i < Vec.size t.heap); *)
  (*   Vec.set t.heap i v *)

  let size t =
    Vec.size t.heap

  let empty t =
    Vec.size t.heap = 0

  let inHeap t i =
    i < Vec.size t.indice && Vec.get t.indice i >= 0


  let percolateUp lt t j =
    let i = ref j in
    let x = Vec.get t.heap !i in
    while !i <> 0 && lt x (Vec.get t.heap (parent !i)) do
      Vec.set t.heap !i (Vec.get t.heap (parent !i));
      Vec.set t.indice (Vec.get t.heap !i) !i;
      i := parent(!i);
    done;
    Vec.set t.heap !i x;
    Vec.set t.indice x !i

  exception Break
  let percolateDown lt t j =
    let i = ref j in
    let x = Vec.get t.heap !i in
    begin try
        while left !i < Vec.size t.heap do
          let child =
            if (right !i < Vec.size t.heap) &&
               lt (Vec.get t.heap (right !i)) (Vec.get t.heap (left !i)) then
              right !i else left !i
          in
          if not (lt (Vec.get t.heap child) x) then raise Break;
          Vec.set t.heap !i (Vec.get t.heap child);
          Vec.set t.indice (Vec.get t.heap !i) !i;
          i := child
        done;
      with Break -> ()
    end;
    Vec.set t.heap !i x;
    Vec.set t.indice x !i


  let print t =
    Printf.eprintf "\n%!";
    for i = 0 to size t -1 do
      Printf.eprintf "%d:%d, %!" (Vec.get t.indice i) (Vec.get t.heap i)
    done;
    Printf.eprintf "\n%!"


  let rec heapProperty t i =
    i >= Vec.size t.heap || (i = 0 || (Vec.get t.heap i >= Vec.get t.heap (parent i)) && heapProperty t (left i) && heapProperty t (right i))

  let insert lt t n =
    (* Printf.eprintf " Heap Insert %d\n%!" n; *)
    Vec.growTo t.indice (n+1) (-1);
    assert (not (inHeap t n));
    Vec.set t.indice n (Vec.size t.heap);
    Vec.push t.heap n (-1);
    percolateUp lt t (Vec.get t.indice n)

  let removeMin lt t =
    (* Printf.eprintf " Heap remove min\n%!"; *)
    (* print t; *)
    (* Printf.printf "heap size %d\n%!" (Vec.size t.heap); *)
    let x = Vec.get t.heap 0 in
    Vec.set t.heap 0 (Vec.last t.heap);
    Vec.set t.indice (Vec.get t.heap 0) 0;
    Vec.set t.indice x (-1);
    Vec.pop t.heap;
    if Vec.size t.heap > 1 then percolateDown lt t 0;
    (* Printf.eprintf " end Heap remove min %d\n%!" x; *)
    x

  let decrease lt t n =
    assert (inHeap t n);
    percolateUp lt t (Vec.get t.indice n)

  let increase lt t n =
    assert (inHeap t n);
    percolateDown lt t (Vec.get t.indice n)

  let update lt t n =
    if not (inHeap t n) then
      insert lt t n
    else
      (percolateUp lt t (Vec.get t.indice n);
       percolateDown lt t (Vec.get t.indice n))

  let clear t dummy =
    Vec.clear t.indice (-1) ;
    Vec.clear t.heap dummy

end
