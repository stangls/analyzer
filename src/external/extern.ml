(* Processing of external information of other tools *)

open Externtypes
open Cil

(* predefined dummy-values instead of reading actual file *)
module DummyInvariantsReader : InvariantsReader =
struct
  type t = invariant list

  let of_c_file fn =
    Some [
      (
        Position (fn,6,0) , [
          ( { name="x"; (*ctype="int";*) } , Interval(0,1) )
        ]
      )
    ]

end

(* reads invariants from appropriate .ix xml files *)
module IxFileInvariantsReader : InvariantsReader =
struct
  type t = invariant list
  open Xml

  let of_xml_file fn cfn =
    let get_element tagName xml =
      let selector = function
        | Element (tn,_,_) -> tn=tagName
        | _ -> false
      in List.find selector (children xml)
    in let element_value xml =
      let data=pcdata ( List.hd (children xml) )
      in Printf.printf "data : %s\n" data; data
    in try
      let convert_invariants invs (node:xml) : t =
        if (tag node)="invariant" then begin
          let loc=ref None
          in let convert_var_invariants (var_invariants:var_invariant list) node : var_invariant list = 
            let nodeTag=tag node
            in if nodeTag="location" then begin
              loc:=Some (Position(
                cfn,
                int_of_string (element_value (get_element "line" node)),
                int_of_string (element_value (get_element "column" node))
              ));
              var_invariants
            end else if nodeTag="variable" then begin
              let name=ref None in let value=ref None
              in let convert_variable node =
                let nodeTag=tag node
                in if nodeTag="name" then name:=Some (element_value node)
                else if nodeTag="interval" then
                  value:=Some (Interval(
                    int_of_string (element_value (get_element "lower-bound" node)),
                    int_of_string (element_value (get_element "upper-bound" node))
                  ))
              in begin
                iter convert_variable node;
                match (!name,!value) with
                | Some name, Some value -> ({name=name},value)::var_invariants
                | Some name, None -> begin
                    Printf.printf "WARNING: Encountered malformed variable %s !\n" name;
                    var_invariants
                  end
                | None, _ -> begin
                    Printf.printf "WARNING: Encountered malformed variable !\n";
                    var_invariants
                  end
              end
            end else
              var_invariants
          in let var_invariants=fold convert_var_invariants [] node
          in match !loc with
          | None -> Printf.printf "WARNING: Invariant missing location!" ; invs
          | Some loc -> (loc,var_invariants)::invs
        end else
          invs
      in let xml = parse_file fn
      in Some ( fold convert_invariants [] xml )
    with
      Error(error,pos) -> begin
        Printf.printf "ERROR parsing %s at line %d: %s\n" fn (line pos) (error_msg error);
        None
      end
    | File_not_found _ -> None

  let of_c_file fn =
    of_xml_file (Str.replace_first (Str.regexp "\\.[^.]*$") ".ix" fn) fn

end

(* heart of the file: the invariants manipulator *)
module Invariants : Manipulator =
struct
  type t = invariant list
  type vi = var_invariant list

  open Liveness
  open Cil

  let filter_pos invs f l1 c1 l2 c2 =
    let map ((vis,ts):vi*t) (inv:invariant) : vi*t =
      match inv with
        | ( (Position (pf,pl,pc)), v ) when
          (pl>l1 || pl=l1 && pc>c1 ) && (pl<l2 || pl=l2 && pc<=c2) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
        | _ -> (vis,inv::ts)
    in List.fold_left map ([],[]) invs

  let filter_func invs f func =
    let map (vis,ts) inv =
      match inv with
        | ( (FunctionEntry (pf,pfunc)), v ) when (String.compare pfunc func = 0 ) && (String.compare pf f = 0) -> (v@vis,ts) (* todo: use string hashes *)
        | _ -> (vis,inv::ts)
    in List.fold_left map ([],[]) invs

  let print_location loc ~name =
    ()(*Printf.printf "Location: line %d , byte %d, file %s (%s)\n" loc.line loc.byte loc.file name*)

  class expr_converter_visitor (invariants:invariant list) =
    object (this)
      inherit nopCilVisitor
      val mutable invs = invariants
      val mutable exprs = []
      val mutable prev_line = 0
      val mutable recent_liveset = None
      method result = exprs
      (* create cil expressions from list of matched var_invariants at location loc with known liveset of variables. *)
      method matched (* (var_invariants,filtered_invariants) loc *) = begin
        match recent_liveset with
        | Some liveset -> fun (var_invariants,filtered_invariants) loc -> begin
          invs <- filtered_invariants;
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
                Printf.printf "WARNING: unknown variable %s in external invariant\n" var.name
              ;
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
          (* create expressions for list of var_invariants *)
          let rec get_exprs var_invariants : Cil.exp list =
            (* todo: think about: maybe we have to get AND ( OR(var1_exprs), OR(var2_exprs),... ), i.e. sorted by var.name, i.e. use some kind of map *)
            (*       currently it is AND ( var1_expr1, var2_expr1, var1_expr2, OR( var3expr3base1, var3expr3base1 ), OR( var3expr4base1, var3expr4base1 ) ... ) *)
            (*       and should be AND ( OR ( var1_expr1, var1_expr2 ), var2_expr1 , OR( var3expr3base1, var3expr3base1, var3expr4base1, var3expr4base1 ) ... ) *)
            match var_invariants with
              | (var,value)::vis -> begin
                match get_cil_var var with
                  | Some cil_var -> begin
                      match value with
                      | Interval (min,max) ->
                          ( Formatcil.cExp
                            "( %v:var >= %d:min ) %b:and ( %v:var <= %d:max )"
                            [
                              ("var",Fv cil_var);
                              ("min",Fd(min)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                              ("and",Fb LAnd);
                              ("max",Fd(max)); (* todo: check if var type matches cil_var.vtype, use kinteger and %e *)
                            ]
                          ) :: (get_exprs vis)
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
                            | Some x -> x :: (get_exprs vis)
                            | None -> get_exprs vis
                          end
                      | _ -> Printf.printf "Unsupported external type"; []
                   end
                  | None -> get_exprs vis
                end
              | [] ->  []
          in match merge_exprs (get_exprs var_invariants) (Fb LAnd) with
            | None -> ()
            | Some e -> exprs <- (loc,e)::exprs
        end
        (* no liveset? then we don't care about the filtered stuff *)
        | None -> fun _ _ -> Printf.printf "Missing liveset information!\n"
      end
      (* visit global function *)
      method vfunc func = let loc=func.svar.vdecl in begin
        (* todo : function entry points f.svar.vname *)
        recent_liveset <- None;
        print_location loc ~name:"func";
        prev_line <- loc.line;
        computeLiveness func;
        DoChildren
      end
      (* visit statement *)
      method vstmt stmt =
        recent_liveset <- getLiveSet stmt.sid;
        let loc=get_stmtLoc stmt.skind
        in begin
          let cur_line=loc.line in begin
            print_location loc ~name: "stmt";
            this#matched (filter_pos invs loc.file prev_line max_int cur_line max_int) loc;
            prev_line <- cur_line;
            DoChildren
          end
        end
      (* visit instruction *)
      method vinst instr =
        let loc=get_instrLoc instr
        in let cur_line=loc.line in begin
          print_location loc ~name:"instr";
          this#matched (filter_pos invs loc.file prev_line max_int cur_line max_int) loc;
          prev_line <- cur_line;
          SkipChildren
        end
      (* get list of unvisited invariants *)
      method get_invs = invs
    end

  let to_cil_invariant invariants file =
    (* cilVisitor for visiting functions and statements *)
    let visitor = new expr_converter_visitor invariants
    in begin
      (* compute the control flow graph *)
      (*Cfg.clearFileCFG file ;
      Cfg.computeFileCFG file ;*)
      (* create expressions from invariants *)
      visitCilFileSameGlobals ( visitor :> cilVisitor ) file;
      (* show how many invariants did not stick to the code locations visited *)
      let num_untransformed = List.length (visitor#get_invs)
      in if (num_untransformed>0) then Printf.printf "WARNING: %d invariants could not be matched to appropriate code positions!\n" num_untransformed;
      (* return result *)
      visitor#result
    end

end

(* helpers for global access ... *)

(* specify type of invariants reader *)
module IF = IxFileInvariantsReader (*DummyInvariantsReader*)
(* storage for converted variants *)
let loaded_invariants = ref ([]:cil_invariant list)
(* reference to the assertion-function *)
let assert_fun = ref None

(*
  initialize required things for external information injection.
  loads invariants for c-file and convert to cil-format.
  determines a function to be used as assertion-function.
*)
let init merged_AST cFileNames =
  let add_invariants agg cFile = 
    match IF.of_c_file cFile with
      | Some invariants -> (Invariants.to_cil_invariant invariants merged_AST) @ agg
      | None            -> agg
  and filter_assert = function
    | GFun(fundec,_) ->
      ( String.compare fundec.svar.vname "assert" = 0) &&
      ( fundec.svar.vtype = TFun(Cil.voidType,Some ["expression",Cil.intType,[]],false,[]) )
    | GVarDecl(varinfo,_) ->
      ( String.compare varinfo.vname "assert" = 0) &&
      ( varinfo.vtype = TFun(Cil.voidType,Some ["expression",Cil.intType,[]],false,[]) )
    | _ -> false
  in begin
    loaded_invariants := (List.fold_left add_invariants [] cFileNames);
    assert_fun := try
      Some (List.find filter_assert merged_AST.globals)
    with Not_found ->
      Printf.printf "WARNING: No assert(int) function found.\n";
      None
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


