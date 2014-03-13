
open ExternTypes
(*open GobConfig*)

module IxFileInvariantsWriter : InvariantsWriter =
struct
  type t = invariant list
  open Xml

  (* supply a function to print xml as string *)
  let to_string =
    Xml.to_string_fmt (* nice formatting *)
  (*Xml.to_string*) (* small formatting *)

  let create_xml invs =
    let from_invs = [ PCData "not implemented" ]
    in Element ( "ix", [ "version","1.0" ] , from_invs )

  let to_file fn invs =
    let xml_str = to_string_fmt (create_xml invs)
    and chan = open_out fn
    in begin
      output_string chan xml_str;
      close_out chan
    end

end
