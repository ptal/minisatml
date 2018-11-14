module Vec_int : sig
  type t = {
    mutable data : int array;
    (* mutable size : int; *)
    (* mutable cap : int; *)
  }

  val init : int -> int -> t
  val grow : t -> int -> int -> unit
  val growTo : t -> int -> int -> unit
  val size : t -> int
  val shrink : t -> int -> unit
  val pop : t -> unit
  val capacity : t -> int
  val last : t -> int
  val get : t -> int -> int
  val get_data : t -> int array
  val set : t -> int -> int -> unit
  val push : t -> int -> int -> unit

  val selectionSort : t -> unit
  val sort : (int -> int -> int) -> t -> unit

  val iter : (int -> unit) -> t -> unit
  val iteri : (int -> int -> unit) -> t-> unit
  val find : t -> int -> bool
  val remove : t -> int -> unit
  val copyTo : t -> t -> int -> unit
  val fromList : int list -> int -> t
  val clear : t -> int -> unit
end
