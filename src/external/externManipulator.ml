open ExternTypes
open GobConfig

module M1 : Manipulator = struct

  open Cil
  open Liveness

  type t = invariant list
  type vi = var_invariant list

  let filter_pos invs f l1 c1 l2 c2 =
    let map ((is,ts):t*t) (inv:invariant) : t*t =
      match inv with
        | ( (Position (pf,pl,pc)), v ) when
          (*Printf.printf "filter_pos %s %d,%d ∈? %s %d,%d-%d,%d: \n" pf pl pc f l1 c1 l2 c2;*)
          (pl>l1 || pl=l1 && pc>c1 ) && (pl<l2 || pl=l2 && pc<=c2) && (String.compare pf f = 0) -> (inv::is,ts) (* todo: use string hashes *)
        | _ -> (is,inv::ts)
    in List.fold_left map ([],[]) invs

  let filter_func invs f func =
    let map (is,ts) inv =
      match inv with
        | ( (FunctionEntry (pf,pfunc)), v ) when (String.compare pfunc func = 0 ) && (String.compare pf f = 0) -> (inv::is,ts) (* todo: use string hashes *)
        | _ -> (is,inv::ts)
    in List.fold_left map ([],[]) invs

  let print_location loc ~name =
    () (*Printf.printf "Location: line %d , byte %d, file %s (%s)\n" loc.line loc.byte loc.file name*)

  class expr_converter_visitor (invariants:invariant list) =
    object (this)
      inherit nopCilVisitor
      val mutable invs:invariant list = invariants
      val mutable exprs:(cil_invariant*invariant list) list = []
      val mutable prev_loc:Cil.location = { line=0; file=""; byte=0 }
      val mutable recent_liveset = None
      val mutable filtered_invariants:invariant list = []
      method result = exprs
      (*
        create cil expressions from list of location-matched invariants at location loc with known liveset of variables.
        stores actual result in exprs and returns tuple of list of not translated (=filtered) invariants
      *)
      method matched (* (invariants,unmatcheded_invariants) loc *) = begin
        match recent_liveset with
        | Some liveset -> fun (invariants,unmatched_invariants) loc -> begin
          invs <- unmatched_invariants;
          (* translate a var_invariant to a cil_var *)
          let rec get_cil_var var =
            let cil_var = ref None
            in let possible_cil_var cil_var' =
              if cil_var'.vname=var.name then begin
                cil_var := Some cil_var';
                true (* = stop searching *)
              end else
                false (* = continue searching *)
            in begin
              if not (VS.exists possible_cil_var liveset) then
                Printf.printf "WARNING: unknown variable %s in external invariant for %s at line %d.\n" var.name loc.file loc.line;
              !cil_var
            end
          in
          (* merge list of cil-expressions using binary operand like "Fb LOr" or "Fb LAnd" *)
          let rec merge_exprs (exprs:Cil.exp list) operand : Cil.exp option = 
            let rec merge_exprs' exprs expr = match exprs with
              | e::es -> merge_exprs' es (Formatcil.cExp "( %e:e1 ) %b:op ( %e:e2 )" [ "e1",Fe e;"e2",Fe expr;"op",operand; ])
              | [] -> expr
            in match exprs with
              | [e] -> Some e
              | e::es -> Some (merge_exprs' es e)
              | [] -> None
          in
          (* create expressions from list of invariants and also return used and unused invariants (which have been/have not been translated to expressions for some reason) *)
          let rec get_exprs invariants used_invariants filtered_invariants : (Cil.exp list * invariant list * invariant list ) =
            match invariants with
              | (inv_loc,var_invariants)::vis -> begin
                let rec get_exprs_vi var_invariants used_invariants filtered_invariants = match var_invariants with
                  | (var,value)::vis -> begin
                    (* Printf.printf "get_exprs_vi of\n%s\n" ( Pretty.sprint ~width:80 ( d_var_invariant (var,value) ) );*)
                    match get_cil_var var with
                      | Some cil_var -> begin
                          match value with
                          (* interval special case : single value *)
                          | Interval (min,max) when min=max ->
                              let (exprsFollowing,viUsed,viFiltered)=get_exprs_vi vis used_invariants filtered_invariants
                              in
                                (
                                  ( Formatcil.cExp
                                    "%v:var == %d:val"
                                    [
                                      ("var",Fv cil_var);
                                      ("val",Fd(min)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                                    ]
                                  ) :: exprsFollowing,
                                  ((inv_loc,[(var,value)])::viUsed), viFiltered
                                )
                          (* interval *)
                          | Interval (min,max) ->
                              let (exprsFollowing,viUsed,viFiltered)=get_exprs_vi vis used_invariants filtered_invariants
                              in
                                (
                                  ( Formatcil.cExp
                                    "( %v:var >= %d:min ) %b:and ( %v:var <= %d:max )"
                                    [
                                      ("var",Fv cil_var);
                                      ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                                      ("and",Fb LAnd);
                                      ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                                    ]
                                  ) :: exprsFollowing,
                                  ((inv_loc,[(var,value)])::viUsed), viFiltered
                                )
                          (* pointer *)
                          | Pointer (bases,min,max) ->
                              let rec exprs_for_bases bases agg : Cil.exp list = match bases with
                                | base::bs -> (
                                    match base with 
                                    | Null ->
                                      let expr =
                                        Formatcil.cExp
                                          "%v:var = NULL"
                                          [
                                            ("var",Fv cil_var);
                                          ]
                                       in exprs_for_bases bs (expr::agg)
                                    | Variable base_var ->
                                      let agg'=match get_cil_var base_var with
                                        | Some cil_base_var ->
                                          let expr = 
                                            Formatcil.cExp
                                              "( %v:var >= ( %v:base_var + %d:min ) ) %b:and ( %v:var <= ( %v:base_var + %d:max ) )"
                                              [
                                                ("var",Fv cil_var);
                                                ("base_var",Fv cil_base_var);
                                                ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                                                ("and",Fb LAnd);
                                                ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                                              ]
                                           in expr::agg
                                         | None -> agg
                                       in exprs_for_bases bs agg'
                                    | Invalid -> [] (* todo : invalid means what exactly? we know nothing? *)
                                  )
                                | [] -> agg
                              in begin
                                match ( merge_exprs (exprs_for_bases bases []) (Fb LOr) ) with
                                | Some x ->
                                  let (exprsFollowing,viUsed,viFiltered)=get_exprs_vi vis used_invariants filtered_invariants
                                  in ( x :: exprsFollowing, ((inv_loc,[(var,value)])::viUsed), viFiltered )
                                | None -> begin
                                    (*Printf.printf "filtered\n";*)
                                    get_exprs_vi vis used_invariants ((inv_loc,[(var,value)])::filtered_invariants)
                                  end
                              end
                          | _ -> begin
                              Printf.printf "ERROR: Unsupported external type";
                              get_exprs_vi vis used_invariants ((inv_loc,[(var,value)])::filtered_invariants)
                            end
                      end
                      | None -> begin
                          (*Printf.printf "filtered\n";*)
                          get_exprs_vi vis used_invariants ((inv_loc,[(var,value)])::filtered_invariants)
                        end
                  end
                  | [] ->  ([],used_invariants,filtered_invariants)
                  (* end get_exprs_vi *)
                in get_exprs_vi var_invariants used_invariants filtered_invariants
              end
              | [] ->  ([],used_invariants,filtered_invariants)
              (* end get_exprs *)
          in let (exprsCreated,used_invariants,filtered_invariants) = get_exprs invariants [] []
          in begin begin
              match merge_exprs exprsCreated (Fb LAnd) with
              | None -> ()
              | Some e -> exprs <- ((loc,e),used_invariants)::exprs
            end;
            filtered_invariants
          end
        end
        (* no liveset? then we don't care about the filtered stuff *)
        | None -> fun _ _ -> begin
            Printf.printf "ERROR: Missing liveset information!\n";
            []
          end
      end
      method tryMatch loc =
        begin
          (* if we moved to another file, we start from the top of the file *)
          if loc.file<>prev_loc.file then begin
            prev_loc <- { line=0 ; byte=0 ; file=loc.file }
          end;
          (* more than one matching in one line with different byte offset ⇒ filter out already translated invariants of this line *)
          if loc.file=prev_loc.file && loc.line=prev_loc.line && loc.byte<>prev_loc.byte then begin
            let oops = ref 0 in begin
              exprs <- List.filter ( fun ((l,e),original_invariants) ->
                    if l.line=loc.line && l.file=loc.file then begin
                      (*Printf.printf "filtered %s\n" ( Pretty.sprint ~width:80 ( d_cil_invariant (l,e) ) );*)
                      oops:=!oops+1;
                      filtered_invariants <- original_invariants@filtered_invariants;
                      false
                    end else true
                ) exprs;
              if !oops>0 then begin
                Printf.printf "WARNING: more than one location suitable for external invariants in %s line %i (maybe two instructions in one line?)\n" loc.file loc.line;
                Printf.printf "         had to ignore %d invariants because I'm not able to handle this (yet?).\n" !oops;
              end
            end
          end else begin
            let filtered_invariants' = this#matched (filter_pos invs loc.file prev_loc.line max_int loc.line max_int) loc
            in filtered_invariants<-filtered_invariants'@filtered_invariants
          end;
          (* store this location, so we try to match from this location on in the next run (if we stay in current file, see above) *)
          prev_loc <- loc
        end
      (* visit global function: compute liveness of that function & set prev_loc *)
      method vfunc func = let loc=func.svar.vdecl in begin
        (* todo : function entry points f.svar.vname *)
        recent_liveset <- None;
        print_location loc ~name:"func";
        prev_loc <- loc;
        computeLiveness func;
        DoChildren
      end
      (* visit statement *)
      method vstmt stmt =
        recent_liveset <- getLiveSet stmt.sid;
        let loc=get_stmtLoc stmt.skind
        in begin
          print_location loc ~name: "stmt";
          this#tryMatch loc;
          DoChildren
        end
      (* visit instruction *)
      method vinst instr =
        let loc=get_instrLoc instr
        in begin
          print_location loc ~name:"instr";
          this#tryMatch loc;
          SkipChildren
        end
      (* get list of unvisited invariants *)
      method get_invs = invs
      (* get number of invariants filtered away *)
      method get_filtered_invs = filtered_invariants
    end

  let transform_to_cil invariants file =
    (* cilVisitor for visiting functions and statements *)
    let visitor = new expr_converter_visitor invariants
    in begin
      (* create expressions from invariants *)
      visitCilFileSameGlobals ( visitor :> cilVisitor ) file;
      (* show invariants not mapped to cil-expressions *)
      let num_position_unmatched = Helper.num_var_invariants visitor#get_invs
      and num_filtered = List.length (visitor#get_filtered_invs)
      in if num_position_unmatched>0 || num_filtered>0 then begin
        Printf.printf "WARNING: %d invariants seem not related to code positions at all and %d could not be translated to cil expressions!\n" num_position_unmatched num_filtered;
        if (get_bool "dbg.verbose") then begin Printf.printf "Not matching with code : \n%s\n" ( Pretty.sprint ~width:80 ( Pretty.docList d_invariant () (visitor#get_invs) ) );
          Printf.printf "Not translated : \n%s\n" ( Pretty.sprint ~width:80 ( Pretty.docList d_invariant () (visitor#get_filtered_invs) ) );
        end
      end;
      (* return result *)
      visitor#result
    end

end
