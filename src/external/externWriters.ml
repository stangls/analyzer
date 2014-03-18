
open ExternTypes
(*open GobConfig*)

module IxFileInvariantsWriter : InvariantsWriter =
struct
  type t = invariant list
  open Xml

  (* supply a function to print xml as string *)
  let to_string =
    Xml.to_string_fmt (* nice formatting *)
    (*Xml.to_string (* small formatting *)*)

  let rel_path f =
    let firstSlash = Str.regexp "^\\/"
    in Str.replace_first firstSlash "" f

  let xml_simple tagName content = Element( tagName, [], [PCData(content)] )

  let xml_location loc =
    Element(
      "location", [],
      match loc with
      | Position(file,line,col) ->
        [
          xml_simple "file" (rel_path file);
          xml_simple "line" (string_of_int line);
          xml_simple "column" (string_of_int col)
        ]
      | FunctionEntry(file,func,line,col) ->
        [
          xml_simple "file" (rel_path file);
          xml_simple "function-entry" func;
          xml_simple "line" (string_of_int line);
          xml_simple "column" (string_of_int col)
        ]
    )

  let xml_value v =
    match v with
    | Interval(min,max) ->
      Some (Element(
        "interval", [],
        [ xml_simple "lower-bound" (Int64.to_string min)
        ; xml_simple "upper-bound" (Int64.to_string max) ]
      ))
    | _ -> None

  let xml_var_inv agg (var,value) =
    let xml_value = xml_value value
    in match xml_value with
    | None -> agg
    | Some xml_value -> 
        Element(
          "variable", [],
          [
            xml_simple "name" var.name;
            (*xml_simple "type" "int"*) (* todo: types *)
            xml_value
          ]
        ) :: agg

  let xml_inv agg (loc,var_invs) =
    let xml_var_invs = List.fold_left xml_var_inv [] var_invs
    in match xml_var_invs with
    | [] -> agg
    | _ ->
      Element(
        "invariant",
        [],
        (xml_location loc) :: xml_var_invs
      )::agg

  let create_xml invs =
    let xml_invs = List.fold_left xml_inv [] invs
    in Element (
        "ix",
        [ "version","1.0" ] ,
        xml_simple "tool" "goblint" :: xml_invs
      )

  let to_file fn invs =
    let xml_str = to_string_fmt (create_xml invs)
    and chan = open_out fn
    in begin
      output_string chan xml_str;
      close_out chan
    end

end
