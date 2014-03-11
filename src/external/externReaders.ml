
open ExternTypes
open GobConfig

(* predefined dummy-values instead of reading actual file *)
module DummyInvariantsReader : InvariantsReader =
struct
  type t = invariant list

  let of_c_file fn =
    Some [
      (
        Position (fn,6,0) , [
          ( { name="x"; (*ctype="int";*) } , Interval(0L,1L) )
        ]
      )
    ]
  let of_file fn = None

end

(* reads invariants from appropriate .ix xml files *)
module IxFileInvariantsReader : InvariantsReader =
struct
  type t = invariant list
  open Xml

  let noFileEndRegexp = Str.regexp "\\.[^.]*$"
  let fileRegexp = Str.regexp "/[^/]*$"
  let noValidVarNameChar = Str.regexp "[^A-Za-z0-9_].*$"

  let get_element tagName xml =
    let selector = function
      | Element (tn,_,_) -> tn=tagName
      | _ -> false
    in List.find selector (children xml)
  let get_elements tagName xml =
    let selector = function
      | Element (tn,_,_) -> tn=tagName
      | _ -> false
    in List.filter selector (children xml)
  let element_value xml = pcdata ( List.hd (children xml) )

  (* we do not clean variable-names anymore, invalid names are actually invalid *)
  let clean_var_name vn =
    (*try Str.string_before vn ( Str.search_forward noValidVarNameChar vn 0 ) with Not_found -> vn*)
    vn

  exception No_location
  let of_xml_file fn cfn =
    let path=try Str.string_before fn ( Str.search_forward fileRegexp fn 0 ) with Not_found -> ""
    in
    try
      let convert_invariants invs (node:xml) : t =
        if (tag node)="invariant" then begin
          let loc=ref None
          in let convert_var_invariants (var_invariants:var_invariant list) node : var_invariant list = 
            let nodeTag=tag node
            in if nodeTag="location" then begin
              (* fault-tolerant location-interpretation *)
              let file =
                try Some (path ^ "/" ^ element_value (get_element "orig-file" node)) with Not_found ->
                try Some (path ^ "/" ^ element_value (get_element "org-file" node)) with Not_found ->
                try Some (path ^ "/" ^ element_value (get_element "file" node)) with Not_found -> cfn
              and line =
                try Some( int_of_string (element_value (get_element "orig-line" node)) ) with Not_found ->
                try Some( int_of_string (element_value (get_element "org-line" node)) ) with Not_found ->
                try Some( int_of_string (element_value (get_element "line" node)) ) with Not_found -> None
              and column =
                try Some( int_of_string (element_value (get_element "orig-column" node)) ) with Not_found ->
                try Some( int_of_string (element_value (get_element "org-column" node)) ) with Not_found ->
                try Some( int_of_string (element_value (get_element "column" node)) ) with Not_found -> None
              in begin
                match file,line,column with
                | Some file, Some line, Some col ->
                  loc:=Some (Position( file, line, col ))
                | Some file, Some line, None ->
                  loc:=Some (Position( file, line, 0 ))
                | _ -> ()
              end;
              var_invariants
            end else if nodeTag="variable" then begin
              let name=ref None in let value=ref None
              in let convert_variable node =
                let nodeTag=tag node
                in if nodeTag="name" then name:=Some (clean_var_name (element_value node))
                else if nodeTag="interval" then
                  try
                    value:=Some (Interval(
                      Int64.of_string (element_value (get_element "lower-bound" node)),
                      Int64.of_string (element_value (get_element "upper-bound" node))
                    ))
                  with Not_found -> ()
                else if nodeTag="pointer" then
                  try
                    let interval=get_element "interval" node
                    in let mk_pointer_base element = match (element_value element) with
                      | "NULL" -> Null
                      | "INVALID" -> Invalid
                      | x -> Variable {name=clean_var_name x}
                    in
                      value:=Some (Pointer(
                        List.map mk_pointer_base (get_elements "base" node),
                        Int64.of_string (element_value (get_element "lower-bound" interval)),
                        Int64.of_string (element_value (get_element "upper-bound" interval))
                      ))
                  with Not_found -> ()
              in begin
                iter convert_variable node;
                match (!name,!value) with
                | Some name, Some value -> ({name=name},value)::var_invariants
                | Some name, None -> begin
                    Printf.printf "WARNING: Encountered malformed variable %s !\n" name;
                    var_invariants
                  end
                | _ -> begin
                    Printf.printf "WARNING: Encountered malformed variable !\n";
                    var_invariants
                  end
              end
            end else
              var_invariants
          in let var_invariants=fold convert_var_invariants [] node
          in match !loc with
          | None -> Printf.printf "WARNING: Invariant missing location!\n" ; invs
          | Some loc -> (loc,var_invariants)::invs
        end else
          invs
      in let xml = parse_file fn
      in
        if (get_bool "dbg.debug") then Printf.printf "Reading %s\n" fn;
        Some ( fold convert_invariants [] xml )
    with
      Error(error,pos) -> begin
        Printf.printf "ERROR parsing %s at line %d: %s\n" fn (line pos) (error_msg error);
        None
      end
    | File_not_found _ -> None

  let of_c_file fn =
    of_xml_file (Str.replace_first noFileEndRegexp ".ix" fn) (Some fn)

  let of_file fn =
    of_xml_file fn None

end

