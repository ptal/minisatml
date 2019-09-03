
type 'a t = {
  heap : 'a Vec.t;
  indice : int Vec.t;
}
val init : 'a -> 'a t

val reinit : 'a t -> int -> 'a -> unit

val left : int -> int
val right : int -> int
val parent : int -> int

(* val get : 'a t -> int -> 'a *)
(* val set : 'a t -> int -> 'a -> unit *)
val percolateUp : (int -> int -> bool) -> int t -> int -> unit
val percolateDown : (int -> int -> bool) -> int t -> int -> unit
val heapProperty : 'a  t -> int -> bool
val size : 'a t -> int
val empty : 'a t -> bool
val inHeap : int t -> int -> bool

val decrease : (int -> int -> bool) -> int t -> int -> unit
val increase : (int -> int -> bool) -> int t -> int -> unit
val insert : (int  -> int -> bool) -> int t -> int -> unit
val removeMin : (int -> int -> bool) -> int t -> int
val update : (int -> int -> bool) -> int t -> int -> unit

val clear : 'a t -> 'a -> unit

val print : int t -> unit
