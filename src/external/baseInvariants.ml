
open ExternTypes

(* we have to talk about the same domains as the base analysis *)
(*module VD = BaseDomain.Main.VD*)
module VD = ValueDomain.Compound

module BaseDomainHandler : (ValueDomainHandler with type t = VD.t) = struct
  type t = VD.t

  (* we have to talk about the same domains as the base-analysis *)
  module ID = ValueDomain.ID
  module AD = ValueDomain.AD

  (* merge invariants of base-domain with join-operator (todo : write about contexts) *)
  let merge = VD.join

  (* create an external value *)
  let to_value (x:t) = match x with
  | `Int i -> ID.to_ext_value i
  | `Top | _ -> Undefined

end

type ctx = (BaseDomain.Dom.t,VD.t) Analyses.ctx
type st = ( ctx -> Cil.varinfo -> VD.t ) * ctx

(*
  storage for base-invariants.
  relies on a hashtable invariants creator.
*)

module S : (InvariantsCreationHelper with type t = st ) = struct
  type t = st

  module BIC = HashtblInvariantsCreator.S( BaseDomainHandler )

  (* base-invariants *)
  let baseInvariants = BIC.create ()

  (*
    create invariants within some HashTblInvariantsCreator.
    the value comes from the base-analysis' right-hand side evaluation.
  *)
  let store loc function_entry ((var_get,ctx):t) : unit =
    let handle_entry var vd_value =
      let value = var_get ctx var
      in BIC.add baseInvariants loc function_entry var value
    and (cpa,_) = ctx.local
    in BaseDomain.CPA.iter (handle_entry) cpa

  let get_invariants (_:unit) = BIC.retrieve baseInvariants

end
