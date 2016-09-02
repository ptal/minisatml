module Vec : sig
  type 'a t = {
    mutable data : 'a array;
    mutable size : int;
    mutable cap : int;
  }

  val init : int -> 'a -> 'a t
  val grow : 'a t -> int -> 'a -> unit
  val size : 'a t -> int
  val shrink : 'a t -> int -> unit
  val pop : 'a t -> unit
  val capacity : 'a t -> int
  val last : 'a t -> 'a
  val get : 'a t -> int -> 'a
  val get_data : 'a t -> 'a array
  val set : 'a t -> int -> 'a -> unit
  val push : 'a t -> 'a -> 'a -> unit

  val selectionSort : 'a t -> unit
  val sort : 'a t -> unit

  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t-> unit
  val find : 'a t -> 'a -> bool
  val remove : 'a t -> 'a -> unit
  val copyTo : 'a t -> 'a t -> 'a -> unit
  val fromList : 'a list -> int -> 'a t
  val clear : 'a t -> 'a -> unit
end
