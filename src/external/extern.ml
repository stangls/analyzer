(* Processing of external information of other tools *)

open Cil
open ExternTypes
open ExternReaders

(* specify reader *)
module IF = IxFileInvariantsReader (*DummyInvariantsReader*)
(* storage for converted variants *)
let loaded_invariants = ref ([]:cil_invariant list)
(* reference to the assertion-function *)
let assert_type = TFun(Cil.voidType,Some ["expression",Cil.intType,[]],false,[])
let assert_fun = ref None
let intern_assert = makeGlobalVar "goblint's internal assert function" assert_type
let use_intern_assert = true

(*
  initialize required things for external information injection.
  loads invariants for c-file and convert to cil-format.
  determines a function to be used as assertion-function.
*)
let init merged_AST cFileNames =
  let add_invariants agg cFile = 
    match IF.of_c_file cFile with
      | Some invariants -> (ExternInvariants.to_cil_invariant invariants merged_AST) @ agg
      | None            -> agg
  in let filter_assert = function
    | GVarDecl(varinfo,_) when ( String.compare varinfo.vname "assert" = 0) && ( varinfo.vtype = assert_type ) ->
      assert_fun:=Some varinfo; true
    | _ -> false
  in begin
    loaded_invariants := (List.fold_left add_invariants [] cFileNames);
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


