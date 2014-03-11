
open ExternTypes

(* we have to talk about the same domains as the base analysis *)
(*module VD = BaseDomain.Main.VD*)
module VD = ValueDomain.Compound

module BaseDomainHandler : (ValueDomainHandler with type t = VD.t) = struct
  type t = VD.t

  (* we have to talk about the same domains as the base-analysis *)
  module ID = ValueDomain.ID
  module AD = ValueDomain.AD

  (* merge invariants of base-domain by with join-operator *)
  let merge = VD.join
  let to_value (x:t) = match x with
  | `Int i -> ID.to_ext_value i
  | `Top | _ -> Undefined

end

module S : (InvariantsCreationHelper with type t = BaseDomain.Dom.t) = struct
  type t = BaseDomain.Dom.t

  module BIC = HashtblInvariantsCreator( BaseDomainHandler )

  (* base-invariants *)
  let baseInvariants = BIC.create ()

  (* create invariants within some HashTblInvariantsCreator *)
  let store (d:t) : unit =
      let (cpa,flags) = d in begin
        (* find the location for these new invariants *)
        let loc = !Tracing.current_loc
        in BaseDomain.CPA.iter (BIC.add baseInvariants loc) cpa
      end

  let get_invariants (_:unit) =
    BIC.retrieve baseInvariants

end
