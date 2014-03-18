
(*
  Processing of external information of other tools.
  This is the main file providing all required functionality to Goblint as an interface (see extern.mli), hiding actual implementations of readers, writers and transformers.
*)

open Cil
open ExternTypes
open ExternReaders
open ExternManipulator
open ExternWriters
open GobConfig
open MyCFG

(* specify reader *)
module IR = IxFileInvariantsReader (*DummyInvariantsReader*)
(* specify modifier *)
module M = M1
(* specify writer *)
module IW = IxFileInvariantsWriter

exception InternalError

(* storage for converted variants *)
let loaded_invariants = ref ([]:cil_invariant list)
(* type of assertion-functions *)
let assert_type = TFun(Cil.voidType,Some ["expression",Cil.intType,[]],false,[])
(* artificial assertion function only used internally *)
let intern_assert = makeGlobalVar "goblint's internal assert function" assert_type
(* use internal assertion? if not, will use normal assertion-function (if available)
   should be true, so Goblint can distinguish between normal assertions and injected assertions *)
let use_intern_assert = true 
(* the actual assertion function used for injection of invariants *)
let assert_fun = ref None
(* group the read invariants by var-name with Or? *)
let group_invariants = true
(* verify invariants *)
let verify_invariants = true

(*
  initialize required things for external invariants.
  loads invariants for c-files and invariants from file given on command-line and converts them into
  cil-expressions for later injection.
  also determines a function to be used as assertion-function for injection (configuration see above)
  confuration parameters:
    ext_read : boolean      read actual external information (to be injected)
    ext_readFile : string   (additional) filename to read from with the configured readers (see above)
*)
let init merged_AST cFileNames =
  let aggregate_cil_invariants inv_getter agg get_param : cil_invariant list = 
    match inv_getter get_param with
      | Some invariants ->
        if get_bool "dbg.verbose" then begin
          Printf.printf "Read %d external invariants from %s.\n" ( List.length invariants ) get_param;
          Printf.printf "This corresponds to %d total variable-invariants (multiple in one location had been concatenated with AND to one external invariant).\n" (Helper.num_var_invariants invariants);
          Printf.printf "  and %d total vi-values.\n" (Helper.num_var_values invariants);
          (*Printf.printf "%s\n" ( Pretty.sprint ~width:80 ( Pretty.docList d_invariant () (invariants) ) );*)
        end;
        let invariants =
          if group_invariants then
            let grouped_invariants = M.group_by_variables invariants
            in begin
              if get_bool "dbg.verbose" then begin
                Printf.printf "Grouped together to %d total variable-invariants (multiple in one location with same variable name have been concatenated with OR to one external invariant).\n" (Helper.num_var_invariants grouped_invariants);
                Printf.printf "  and %d total vi-values.\n" (Helper.num_var_values grouped_invariants);
                (*Printf.printf "%s\n" ( Pretty.sprint ~width:80 ( Pretty.docList d_invariant () (grouped_invariants) ) );*)
              end;
              grouped_invariants
            end
          else
            invariants
        in let (cil_invariants,transformed_invariants) = List.split (M.transform_to_cil invariants merged_AST)
        in
          if get_bool "dbg.verbose" then begin
            let transformed_invariants = List.concat transformed_invariants
            in let num_transformed = Helper.num_var_invariants ( transformed_invariants )
            in let num_v_transformed = Helper.num_var_values ( transformed_invariants )
            in Printf.printf "From the above, %d assert-expressions could be developed from %d invariants ( %d values ).\n" ( List.length cil_invariants ) num_transformed num_v_transformed;
          end;
          cil_invariants @ agg
      | None            -> agg
  in let create_exp var = Cil.Lval(Cil.Var var,Cil.NoOffset)
  in let filter_assert = function
    | GVarDecl(varinfo,_) when ( String.compare varinfo.vname "assert" = 0) && ( varinfo.vtype = assert_type ) ->
      assert_fun:= Some (create_exp varinfo); true
    | _ -> false
  in begin
    if get_bool "ext_read" then
      loaded_invariants := List.fold_left (aggregate_cil_invariants IR.of_c_file) (aggregate_cil_invariants IR.of_file [] (get_string "ext_readFile")) cFileNames;
    if use_intern_assert then begin
      assert_fun := Some (create_exp intern_assert)
    end else begin
      try
        ignore (List.find filter_assert merged_AST.globals)
      with Not_found ->
        Printf.printf "WARNING: No assert(int) function found.\n";
    end
  end


(*
  retrieve pre-statement invariants for Cil.location loc in the form of a Cil.expr .
  call [init] first to initialize invariants for given files.
*)
let get_loc_inv_expr (loc_from:Cil.location) (loc:Cil.location) : Cil.exp list =
  if ( loc.line=loc_from.line && (String.compare loc.file loc_from.file = 0) ) then begin
    if get_bool "dbg.verbose" then
      Printf.printf "not getting invariant expressions at %s, because location is unchanged!\n" ( Pretty.sprint ~width:80 (d_loc () loc) );
    []
  end else begin
    if get_bool "dbg.verbose" then
      Printf.printf "getting invariant expressions at %s\n" ( Pretty.sprint ~width:80 (d_loc () loc) );
    let filter_fun (loc',_:cil_invariant) = ( loc.line=loc'.line && (String.compare loc.file loc'.file = 0) )
    in let get_expr (loc',expr) = expr
    in List.map get_expr (List.filter filter_fun !loaded_invariants)
  end

(*
  retrieve assertion expression and list of expressions to assert which should be used
  after the transition from loc_from to loc.
*)
let assertion_exprs (loc_from:Cil.location) (loc:Cil.location) edge : (Cil.exp * Cil.exp list) =
  match !assert_fun with
  | Some assert_fun_exp ->
    begin
      (* only certain edges may have assertions after them *)
      match edge with
      | Assign(_,_) | Proc(_,_,_) | Entry(_) | Test(_,_) | ASM(_) -> assert_fun_exp , get_loc_inv_expr loc_from loc
      | Ret (_,_) | Skip | SelfLoop ->
        if get_bool "dbg.verbose" then
          Printf.printf "not getting invariant expressions at %s for this kind of edge.\n" ( Pretty.sprint ~width:80 (d_loc () loc) );
        assert_fun_exp,[]
    end
  | None -> raise InternalError


(*
  ================= invariant creation ======================
  currently only supports base-domain.
  stores invariants as lattices before writing them.
*)

module BI = BaseInvariants.S

  (* find the location for these new invariants *)
let current_location (_:unit) =
  let loc = !Tracing.next_loc
  in
    Printf.printf "invariant location: %s (byte offset %d)\n" ( Pretty.sprint ~width:80 (d_loc () loc) ) loc.byte;
    loc

(*
  create invariants from base-analysis state
  only if we are in the verifying stage
  and we have a filename to write to
*)
let create_base_invariants var_get ctx =
  if (String.length (get_string "ext_writeFile") != 0) && !Goblintutil.in_verifying_stage then
    BI.store (current_location ()) (var_get,ctx)
type base_ctx = (BaseDomain.Dom.t,BaseDomain.VD.t) Analyses.ctx

let write_invariants (_:unit) : unit =
  if (String.length (get_string "ext_writeFile") != 0) then begin
    Printf.printf "Writing invariants...\n";
    let invs =
      BI.get_invariants ()
    in let (numUndefined,invs) = Helper.filter_undefined_var_invariants invs
    in  begin
      (*List.iter ( fun x -> Printf.printf "Invariant created:\n%s\n" ( Pretty.sprint ~width:80 (d_invariant x) ) ) invs;*)
      Printf.printf "%d invariants created.\n" (List.length invs);
      Printf.printf "%d undefined invariants have been filtered out.\n" numUndefined;
      IW.to_file (get_string "ext_writeFile") invs
    end
  end


