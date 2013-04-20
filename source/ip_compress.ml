(*
 * filename   : ip_compress.ml
 * created at : 2013-04-13 16:25:49
 * author     : Jianing Yang <jianingy.yang AT gmail DOT com>
 *)

open Iprange

let out_format = ref CIDR
let networks = ref []
let set_out_format format =
  if String.compare format "netmask" = 0 then out_format := NETMASK
  else if String.compare format "cidr" = 0 then out_format := CIDR
  else if String.compare format "wildcard" = 0 then out_format := WILDCARD
  else if String.compare format "range" = 0 then out_format := RANGE
  else failwith "invalid output format"
let spec = [
  ("-type", Arg.String set_out_format, "output ip format. valid values are netmask, cidr, wildcard or range.");
]

let read_network () =
  let rec readlines () =
    try
      let line = input_line stdin in
      line :: readlines ()
    with
	End_of_file -> [] in
  readlines ()

let usage = "usage: " ^ Sys.argv.(0) ^ " [-t netmask|cidr|wildcard] ip1 ip2 ..."
let () =
  Arg.parse spec (fun x -> networks := (x :: (!networks))) usage;
  ignore(
    List.map print_endline
      (compress_network ~output_type:!out_format
	 (if List.length !networks > 0 then !networks else (read_network ()))))
