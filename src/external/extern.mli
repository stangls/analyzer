
exception InternalError

(* initialize external invariants with an AST and a list of c-file names *)
val init : Cil.file -> string list -> unit

(* retrieve assertion expression and list of expressions to assert at given location *)
val assertion_exprs : Cil.location -> Cil.exp * Cil.exp list

(* internally used assertion-function *)
val intern_assert : Cil.varinfo

(* create invariants in base analysis *)
type base_ctx = (BaseDomain.Dom.t,BaseDomain.VD.t) Analyses.ctx
val create_base_invariants : ( base_ctx -> Cil.varinfo -> BaseDomain.VD.t ) -> base_ctx -> unit

(* write out all created invariants *)
val write_invariants : unit -> unit

