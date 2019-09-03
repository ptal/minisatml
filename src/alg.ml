let remove t x =
  let j = ref 0 in
  while !j < Vec.size t && Vec.get t !j <> x do incr j done;
  assert (!j < Vec.size t);
  while !j < (Vec.size t - 1) do
    Vec.set t !j (Vec.get t (!j+1));
    incr j
  done;
  Vec.pop t
