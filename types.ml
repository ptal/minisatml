
module Lit : sig
  type t
  val var : t -> int
  type lit = t

  module Array : sig
    type t
    val get : t -> int -> lit
    val size : t -> int
  end

end = struct
  type t = int
  type lit = t

  let var t = t lsr 1

  module Array = struct
    type t = lit array
    let get t i = t.(i)
    let size t = Array.length t
  end

end

module Lbool : sig
  type t
end = struct
  type t =
    | True
    | False
    | Undef
end

module Clause : sig
  type t

  val clause_new : Lit.Array.t -> learnt:bool -> t
  val size : t -> int
  val get_activity : t -> float
  val set_activity : t -> float -> unit
  val abstraction : t -> int

  val shrink : t -> int -> unit
  val pop : t -> unit
  val learnt : t -> bool

  (* Ça a l'air inutile... *)
  (* val set_mark : t -> bool * bool -> unit *)
  (* val get_mark : t -> bool * bool *)
  val last : t -> Lit.t
  val get : t -> int -> Lit.t
  val data : t -> Lit.Array.t (* à retirer si possible *)

end = struct

  type extra =
    | Abst of int  (* int32 dans minisat: sizeof = 4 *)
    | Act of float (* float dans minisat: sizeof = 4 *)

  type t = {
    mutable size : int;
    (* suffisement petit pour pouvoir caser learnt dedans *)
    mutable extra : extra;
    learnt : bool;
    (* dans minisat: casé dans le bit de poids faible de la taille *)

    (* mutable b2 : bool; *)
    (* mutable b3 : bool; *)
    (* dans minisat ce sont les bits 2 et 3 de size_etc *)

    data : Lit.Array.t;
    (* Dans minisat c'est alloué contigue au record *)
  }

  let calc_abstraction ps =
    let abstraction = ref 0 in
    for i = 0 to Lit.Array.size ps - 1 do
      abstraction :=
        !abstraction lor
        (1 lsl ((Lit.var (Lit.Array.get ps i)) land 31))
    done;
    !abstraction

  let clause_new (ps:Lit.Array.t) ~learnt =
    let extra =
      if learnt then Act 0.
      else Abst (calc_abstraction ps)
    in
    { size = Lit.Array.size ps;
      extra;
      data = ps;
      learnt;
      (* b2 = false; *)
      (* b3 = false *)
    }

  let size t = t.size

  let get_activity = function
    | { extra = Act a } -> a
    | _ -> invalid_arg "Clause.activity"

  let set_activity t f =
    t.extra <- Act f

  let abstraction = function
    | { extra = Abst a } -> a
    | _ -> invalid_arg "Clause.abstraction"

  let shrink t i =
    assert(i < size t);
    t.size <- t.size - i

  let pop t = shrink t 1

  let learnt t = t.learnt

  (* let set_mark t (b2, b3) = *)
  (*   t.b2 <- b2; *)
  (*   t.b3 <- b3 *)

  (* let get_mark t = *)
  (*   t.b2, t.b3 *)

  let last t = Lit.Array.get t.data (size t - 1)

  let get t i = Lit.Array.get t.data i

  let data t = t.data

end
