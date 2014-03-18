
open ExternTypes

(*
  creator of invariants (based on some domain d and a hashtable).
  the idea is, that d can be merged for multiple calls of add with same location and same variable
  if it is of the same type (function-entry or not)
*)
module S ( VDM:ValueDomainHandler ) : (InvariantsCreator with type d=VDM.t and type t=( ( Cil.location * string option * Cil.varinfo ) , VDM.t ) Hashtbl.t) = struct

  type t = ( ( Cil.location * string option * Cil.varinfo ) , VDM.t ) Hashtbl.t
  type d=VDM.t

  let create _ = Hashtbl.create 100

  (* add some invarant by merging d into the hashtable *)
  let add tbl loc fentry var value =
    let newVal = 
      try
        let oldVal = Hashtbl.find tbl (loc,fentry,var)
        in VDM.merge oldVal value
      with Not_found -> value
    in Hashtbl.replace tbl (loc,fentry,var) newVal

  let retrieve (tbl:t) =
    (* hashtable loc->(var,value) . we estimate roughly 5 invariants per location *)
    let mappedToLocs = 
      let tmp : ( (Cil.location * string option), ( Cil.varinfo*VDM.t ) list ) Hashtbl.t = Hashtbl.create ((Hashtbl.length tbl)/5)
      in let toTmp ( loc, fentry, var ) value =
        let newVal =
          try
            let oldVal = Hashtbl.find tmp (loc,fentry)
            in (var,value)::oldVal
          with Not_found -> [(var,value)]
        in Hashtbl.replace tmp (loc,fentry) newVal
      in Hashtbl.iter toTmp tbl; tmp
    and toInvs (loc,fentry:Cil.location*string option) (varValList : (Cil.varinfo*VDM.t) list) acc =
      let varVal2varInv (var,value : Cil.varinfo*VDM.t) =
        ( { name=var.vname }, VDM.to_value value )
      and loc = 
        match fentry with
        | None -> Position( loc.file, loc.line, 1 )
        | Some func -> FunctionEntry(  loc.file, func, loc.line, 1 )
      in 
        ( loc, List.rev_map varVal2varInv varValList)::acc
    in Hashtbl.fold toInvs mappedToLocs []

end

