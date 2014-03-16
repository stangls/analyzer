
open ExternTypes

(*
  creator of invariants (based on some domain d and a hashtable).
  the idea is, that d can be merged for multiple calls of add with same location and same variable.
*)
module S ( VDM:ValueDomainHandler ) : (InvariantsCreator with type d=VDM.t and type t=( ( Cil.location * Cil.varinfo ) , VDM.t ) Hashtbl.t) = struct

  type t = ( ( Cil.location * Cil.varinfo ) , VDM.t ) Hashtbl.t
  type d=VDM.t

  let create _ = Hashtbl.create 100

  (* add some invarant by merging d into the hashtable *)
  let add tbl loc var value =
    let newVal = 
      try
        let oldVal = Hashtbl.find tbl (loc,var)
        in VDM.merge oldVal value
      with Not_found -> value
    in Hashtbl.replace tbl (loc,var) newVal

  let retrieve (tbl:t) =
    (* hashtable loc->(var,value) . we estimate roughly 5 invariants per location *)
    let mappedToLocs = 
      let tmp : ( Cil.location, ( Cil.varinfo*VDM.t ) list ) Hashtbl.t = Hashtbl.create ((Hashtbl.length tbl)/5)
      in let toTmp ( loc, var ) value =
        let newVal =
          try
            let oldVal = Hashtbl.find tmp loc
            in (var,value)::oldVal
          with Not_found -> [(var,value)]
        in Hashtbl.replace tmp loc newVal
      in Hashtbl.iter toTmp tbl; tmp
    and toInvs (loc:Cil.location) (varValList : (Cil.varinfo*VDM.t) list) acc =
      let varVal2varInv (var,value : Cil.varinfo*VDM.t) =
        ( { name=var.vname }, VDM.to_value value )
      in 
        (
          Position( loc.file, loc.line, 1 ),
          List.rev_map varVal2varInv varValList
        )::acc
    in Hashtbl.fold toInvs mappedToLocs []

end

