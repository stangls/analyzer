
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

  let noFileEndRegexp = Str.regexp "\\.[^.]*$"
  let fileRegexp = Str.regexp "/[^/]*$"

  (* todo: usages of get_element and get_elements should check for Not_found exception *)

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
              let file = try path ^ "/" ^ element_value (get_element "file" node) with Not_found -> cfn
              in
                loc:=Some (Position(
                  file,
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
                else if nodeTag="pointer" then
                  let interval=get_element "interval" node
                  in let mk_pointer_base element = match (element_value element) with
                    | "NULL" -> Null
                    | "INVALID" -> Invalid
                    | x -> Variable {name=x}
                  in value:=Some (Pointer(
                    List.map mk_pointer_base (get_elements "base" node),
                    int_of_string (element_value (get_element "lower-bound" interval)),
                    int_of_string (element_value (get_element "upper-bound" interval))
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
    of_xml_file (Str.replace_first noFileEndRegexp ".ix" fn) fn

end

