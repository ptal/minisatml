module Lbool : sig
  type t  =
    | LTrue
    | LFalse
    | LUndef

  val bool_of_lbool : t -> bool
  val lbool_of_bool : bool -> t
  val inv : t -> t
  val chap : t -> bool -> t
end

module Lit : sig
  type t = int
  type lit = t

  module Array : sig
    type t = lit array
    val get : t -> int -> lit
    val size : t -> int
    val iter : (lit -> unit) -> t -> unit
    val fold : ('a -> lit -> 'a) -> 'a -> t -> 'a
  end

  val lit : int -> bool -> lit
  val toInt : lit -> int
  val toLit : int -> int
  val tild : lit -> int
  val sign : lit -> bool
  val var : lit -> int
  val unsign : lit -> lit
  val id : lit -> bool -> lit

  val eq : lit -> lit -> bool
  val neq : lit -> lit -> bool
  val lt : lit -> lit -> bool

  (* val int_of_lit : lit -> int *)
  (* val lit_of_int : int -> lit *)
  (* val bool_of_lit : lit -> Lbool.t *)
  (* val lit_of_bool : bool -> lit *)
  (* val lit : int -> bool -> lit *)
  (* val lit_of_int_bool : int -> bool -> lit *)
  (* val comp : lit -> lit -> bool *)
end


module Clause : sig

  type t = {
    mutable size : int;
    (* suffisement petit pour pouvoir caser learnt dedans *)
    mutable act : bool ;
    (* dans minisat: casé dans le bit de poids faible de la taille *)

    (* mutable b2 : bool; *)
    (* mutable b3 : bool; *)
    (* dans minisat ce sont les bits 2 et 3 de size_etc *)

    data : Lit.Array.t;
    (* Dans minisat c'est alloué contigue au record *)
  }

  val clause_new : Lit.Array.t -> int -> learnt:bool -> t
  val size : t -> int
  val get_activity : t -> int
  val set_activity : t -> int -> unit

  (* val shrink : t -> int -> unit *)
  (* val pop : t -> unit *)

  val learnt : t -> bool
  
  (* Ça a l'air inutile... *)
  (* val set_mark : t -> bool * bool -> unit *)
  (* val get_mark : t -> bool * bool *)
  val last : t -> Lit.t
  val get : t -> int -> Lit.t
  val set : t -> int -> Lit.t -> unit
  val data : t -> Lit.Array.t (* à retirer si possible *)
  val iter : (Lit.t -> unit) -> t -> unit
  val fold : ('a -> Lit.t -> 'a) -> 'a -> t -> 'a
end
