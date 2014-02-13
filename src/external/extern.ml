(* Processing of external information of other tools *)

open Cil
open ExternTypes
open ExternReaders
open ExternManipulator
open GobConfig

(* specify reader *)
module IF = IxFileInvariantsReader (*DummyInvariantsReader*)
(* specify modifier *)
module M = M1

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

(*
  initialize required things for external invariants.
  loads invariants for c-files and invariants from file given on command-line and converts them into
  cil-expressions for later injection.
  also determines a function to be used as assertion-function for injection (configuration see above)
  confuration parameters:
    ext_read : boolean      read actual external information (to be injected)
    ext_read_file : string  (additional) filename to read from with the configured readers (see above)
*)
let init merged_AST cFileNames =
  let aggregate_cil_invariants inv_getter agg get_param : cil_invariant list = 
    match inv_getter get_param with
      | Some invariants ->
        if get_bool "dbg.verbose" then begin
          Printf.printf "Read %d external invariants from %s.\n" ( List.length invariants ) get_param;
          Printf.printf "This corresponds to %d total invariants (multiple in one location have been concatenated with AND to one external invariant).\n" (Helper.num_var_invariants invariants);
        end;
        let (cil_invariants,transformed_invariants) = List.split (M.transform_to_cil invariants merged_AST)
        in
          if get_bool "dbg.verbose" then begin
            let num_transformed = List.length( List.concat transformed_invariants )
            in Printf.printf "From the above, %d assert-expressions could be developed from %d invariants.\n" ( List.length cil_invariants ) num_transformed ;
          end;
          cil_invariants @ agg
      | None            -> agg
  in let filter_assert = function
    | GVarDecl(varinfo,_) when ( String.compare varinfo.vname "assert" = 0) && ( varinfo.vtype = assert_type ) ->
      assert_fun:=Some varinfo; true
    | _ -> false
  in begin
    if get_bool "ext_read" then
      loaded_invariants := List.fold_left (aggregate_cil_invariants IF.of_c_file) (aggregate_cil_invariants IF.of_file [] (get_string "ext_readFile")) cFileNames;
    if use_intern_assert then begin
      assert_fun := Some (intern_assert)
    end else begin
      try
        ignore (List.find filter_assert merged_AST.globals)
      with Not_found ->
        Printf.printf "WARNING: No assert(int) function found.\n";
    end
  end

let assert_fun () = !assert_fun

(*
  retrieve pre-statement invariant for Cil.location loc in the form of a Cil.expr
  call [init] first to initialize invariants for given files.
*)
let get_loc_inv_expr (loc:Cil.location) : Cil.exp list =
  let filter_fun (loc',_:cil_invariant) = ( loc.line=loc'.line && (String.compare loc.file loc'.file = 0) )
  in let get_expr (loc',expr) = expr
  in List.map get_expr (List.filter filter_fun !loaded_invariants)


