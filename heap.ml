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
  let parent i = (i-1)/2

  let get t i =
    assert (i < Vec.size t.heap);
    Vec.get t.heap i

  let set t i v =
    assert (i < Vec.size t.heap);
    Vec.set t.heap i v

  let size t =
    Vec.size t.heap

  let empty t =
    Vec.size t.heap = 0

  let inHeap t i =
    i < Vec.size t.indice && Vec.get t.indice i >= 0


  let percolateUp t i =
    let i = ref i in
    let x = get t !i in
    while !i <> 0 && x < Vec.get t.heap !i do
      Vec.set t.heap !i (Vec.get t.heap (parent !i));
      Vec.set t.indice (Vec.get t.heap !i) !i;
      i := parent(!i);
    done;
    Vec.set t.heap !i x;
    Vec.set t.indice x !i

  let percolateDown t j =
    let res = ref true in
    let i = ref j in
    let x = Vec.get t.heap !i in
    while !res && left !i < Vec.size t.heap do
      let child = if (right !i < Vec.size t.heap) && (Vec.get t.heap (right !i) < Vec.get t.heap (left !i)) then right !i else left !i in
      if Vec.get t.heap child >= x then res := false
      else begin
        Vec.set t.heap !i (Vec.get t.heap child);
        Vec.set t.indice (Vec.get t.heap !i) !i;
        i := child
      end
    done;
    Vec.set t.heap !i x;
    Vec.set t.indice x !i

  let rec heapProperty t i =
    i >= Vec.size t.heap || (i = 0 || (Vec.get t.heap i >= Vec.get t.heap (parent i)) && heapProperty t (left i) && heapProperty t (right i))

  let insert t n =
    Vec.grow t.indice (n+1) 0;
    assert (not (inHeap t n));
    Vec.set t.indice n (Vec.size t.heap);
    Vec.push t.heap n 0;
    percolateUp t (Vec.get t.indice n)

  let removeMin t =
    let x = Vec.get t.heap 0 in
    Vec.set t.heap 0 (Vec.last t.heap);
    Vec.set t.indice (Vec.get t.heap 0) 0;
    Vec.set t.indice x (-1);
    Vec.pop t.heap;
    if Vec.size t.heap > 1 then percolateDown t 0;
    x

  let decrease t n =
    assert (inHeap t n);
    percolateUp t (Vec.get t.indice n)

  let increase t n =
    assert (inHeap t n);
    percolateDown t (Vec.get t.indice n)

  let update t n =
    if not (inHeap t n) then
      insert t n
    else
      (percolateUp t (Vec.get t.indice n);
       percolateDown t (Vec.get t.indice n))
end
