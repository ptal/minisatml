open Vec
open Types
open Vec

type var
type polarity

val set_debug : bool -> unit

val newVar_var : bool -> bool -> var
val newVar : unit -> var
val addClause : Lit.t Vec.t -> bool
val simplify : unit -> bool
val solve_lit : out_channel -> Lit.t Vec.t -> bool
val solve : out_channel -> bool
val okay : unit -> bool
val setPolarity : var -> bool -> unit
val setDecisionVar : var -> bool -> unit
val value : var -> Lbool.t
val value : Lit.t -> Lbool.t
val modelValue : Lit.t -> Lbool.t
val nAssigns : unit -> int
val nClauses : unit -> int
val nLearnts : unit -> int
val nVars : unit -> int

val insertVarOrder : var -> unit
val pickBranchLit :  polarity -> float -> Lit.t
val newDecisionLevel : unit -> unit
val uncheckedEnqueue_clause : Lit.t -> Clause.t -> unit
val uncheckedEnqueue : Lit.t -> unit

(* val enqueue_clause : Lit.t -> Clause.t -> Lbool.t *)
(* val enqueue : Lit.t -> Lbool.t *)
val propagate : unit -> Clause.t
val cancelUntil : int -> unit
val analyze : Clause.t ref -> Lit.t Vec.t -> int ref -> unit
val analyzeFinal : Lit.t -> Lit.t Vec.t -> unit
val litRedundant : Lit.t -> int -> bool
val search : out_channel -> int -> int -> Lbool.t
val reduceDB : unit -> unit
val removeSatisfied : Clause.t Vec.t -> unit

val varDecayActivity : unit -> unit
val varBumpActivity : var -> unit
val claDecayActivity : unit -> unit
val claBumpActivity : Clause.t -> unit
val progressEstimate : unit -> float
val attachClause : Clause.t -> unit
val detachClause : Clause.t -> unit
val removeClause : Clause.t -> unit
val locked : Clause.t -> bool
val satisfied : Clause.t -> bool

val decisionLevel : unit -> int
val abstractLevel : var -> int

val printLit : Lit.t -> string
val printClause : Clause.t -> unit
val verifyModel : unit -> unit
val checkLiteralCount : unit -> unit

val printStats : float -> unit
