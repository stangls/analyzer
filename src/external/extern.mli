
exception InternalError

(* initialize external invariants with an AST and a list of c-file names *)
val init : Cil.file -> string list -> unit

(* retrieve assertion expression and list of expressions to assert at given location *)
val assertion_exprs : Cil.location -> Cil.location -> MyCFG.edge -> Cil.exp * Cil.exp list

(* internally used assertion-function *)
val intern_assert : Cil.varinfo

(*
  create invariants in base analysis
  parameters:
    * function to retrieve values of variables
    * context (used to iterate over the variables)
    * None if no function-entry, Some x if it is a function-entry to function f
*)
type base_ctx = (BaseDomain.Dom.t,BaseDomain.VD.t) Analyses.ctx
val create_base_invariants : ( base_ctx -> Cil.varinfo -> BaseDomain.VD.t ) -> base_ctx -> string option -> unit

(* write out all created invariants, the cil file is required for the verification step. *)
val write_invariants : Cil.file -> unit

