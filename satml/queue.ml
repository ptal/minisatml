open Vec

module Queue : sig
  type 'a t

  val insert : 'a t -> 'a -> 'a -> unit
  val peek : 'a t -> 'a
  val pop : 'a t -> unit
  val size : 'a t -> int

  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end = struct
  type 'a t = {
    elems : 'a Vec.t;
    mutable first : int;
  }

  let insert t x dummy = Vec.push t.elems x dummy
  let peek t = Vec.get t.elems t.first
  let pop t = t.first <- t.first + 1
  let size t = Vec.size t.elems
  let get t i = Vec.get t.elems i
  let set t i v = Vec.set t.elems i v

end
